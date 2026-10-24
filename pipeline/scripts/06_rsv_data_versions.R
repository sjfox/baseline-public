# RSV with real-time vs final data: flatline vs drift (raw and square root, 10 most recent
# differences) scored on the series as reported on each forecast date, on the final series cut at the
# same date, and on the real-time series with its last k weeks replaced by final values. Only forecast
# cells available in every data version are compared.
# Output: output/tables/reporting_scores_by_version.csv
source("pipeline/R/setup.R")
source("pipeline/R/analysis.R")
TARGETS_RT <- c(rsv = "rsv_asof", rsv_nhsn = "rsv_nhsn")
MODELS <- CJ(transformation = c("none", "sqrt"), symmetrize = c(TRUE, FALSE), window = 10, sorted = FALSE)
MODELS[, model := model_id(transformation, symmetrize, window)]
cores <- as.integer(Sys.getenv("NCORES", N_CORES))

# Final-data series for a location, ending at the origin, on the same grid as the real-time series
final_series <- function(truth_loc, origin, step) {
  s <- complete_series(truth_loc$date, truth_loc$value, step)
  if (is.null(s)) return(NULL)
  i <- match(origin, s$dates)
  if (is.na(i)) return(NULL)
  s$y[seq_len(i)]
}

# Score the four models for one series (any version) at one origin
score_series <- function(y, task, steps, step, obs, count_data, tag) {
  set.seed(task_seed(task$target, task$location, task$reference_date))
  rbindlist(lapply(seq_len(nrow(MODELS)), function(i) {
    g <- MODELS[i]
    q <- forecast_quantiles_batch(y, g$transformation, g$symmetrize, g$window, keep = steps,
                                  count_data = count_data)[, , 1, drop = TRUE]
    if (is.null(dim(q))) q <- matrix(q, nrow = 1)
    ok <- !is.na(obs)
    if (!any(ok)) return(NULL)
    data.table(model = g$model, horizon = seq_along(steps)[ok], observed = obs[ok],
               median = q[ok, QUANTILES == 0.5], score_quantiles(q[ok, , drop = FALSE], obs[ok]))
  }))[, `:=`(location = task$location, reference_date = task$reference_date, data = tag)][]
}

results <- list()
for (nm in names(TARGETS_RT)) {
  tg <- TARGETS_RT[[nm]]
  d <- load_target(tg)
  if (tg %in% SEASONAL_TARGETS) d$tasks <- d$tasks[month(reference_date) %in% SEASON_MONTHS]
  step <- d$step; steps <- HORIZONS_WEEKLY
  truth <- d$truth[!is.na(value)]; setkey(truth, location, date)
  by_loc <- split(truth, by = "location")

  res <- parallel::mclapply(seq_len(nrow(d$tasks)), function(j) {
    data.table::setDTthreads(1L)
    task <- d$tasks[j]; y <- task$series[[1]]; n <- length(y)
    tl <- by_loc[[task$location]]
    if (is.null(tl)) return(NULL)
    yf <- final_series(tl, task$reference_date, step)
    obs <- truth[.(task$location, task$reference_date + steps * step), value]
    sc <- list(score_series(y, task, steps, step, obs, d$count_data, "real-time"))
    if (!is.null(yf) && !is.na(tail(yf, 1))) {
      sc[[2]] <- score_series(yf, task, steps, step, obs, d$count_data, "final")
      for (k in c(1, 2, 3, 4, 8)) {
        yk <- y; idx <- (n - k + 1):n; yk[idx] <- yf[length(yf) - n + idx]
        if (!anyNA(yk[idx])) sc[[length(sc) + 1]] <- score_series(yk, task, steps, step, obs, d$count_data,
                                                                  paste0("last ", k, " weeks corrected"))
      }
    }
    rbindlist(sc)
  }, mc.cores = cores, mc.preschedule = TRUE)
  results[[nm]] <- rbindlist(res)[, disease := nm]
  message(nm, ": ", nrow(results[[nm]]), " scored forecasts")
}

all_scores <- rbindlist(results)
all_scores[, family := ifelse(grepl("_sym_", model), "flatline", "drift")]
all_scores[, transformation := sub("_.*$", "", model)]

# Pooled over locations (national series excluded), on the cells available in every data version
cmp <- all_scores[in_pooled(TARGETS_RT[disease], location)][
  , n := uniqueN(data), by = .(disease, location, reference_date, horizon)][, .SD[n == max(n)], by = disease]
summ <- cmp[, .(wis = mean(wis), underpred = mean(underpred), overpred = mean(overpred),
                dispersion = mean(dispersion),
                median_bias = median(log((median + 1) / (observed + 1)))), by = .(disease, data, transformation, family)]
fwrite(summ, file.path(OUT_DIR, "tables", "reporting_scores_by_version.csv"))
print(summ)
