# Flatline vs seasonal baseline for wILI at 1-10 weeks ahead.
# Forecast dates: real-time (versioned) wILI, October-April forecast dates from SEASONAL_START. Only
# forecasts whose target week is also in October-April are scored.
# Models: square-root flatline (w = 10 and all data), raw flatline on all data, the seasonal baseline,
# and, each with its own seed, the log flatline on all data and the seasonal model on the most recent
# 3 years.
# Output: output/tables/wili_long_horizon_summary.csv (mean WIS, coverage and % improvement over the
#         seasonal baselines, by model and horizon)
source("pipeline/R/setup.R")
source("pipeline/R/seasonal_model.R")
source("pipeline/R/analysis.R")

H <- 1:10
TREND <- data.table(model = c("sqrt_sym_w10", "sqrt_sym_wall", "none_sym_wall"),
                    transformation = c("sqrt", "sqrt", "none"), window = c(10, Inf, Inf))
LOG_TREND <- data.table(model = "log_sym_wall", transformation = "log", window = Inf)
SEASONAL_3Y <- list(seasonal_last3y = list(n_years = 3))

d <- load_target(ILI_MAIN)
truth <- d$truth[!is.na(value)]; setkey(truth, location, date)
tasks <- d$tasks[reference_date >= SEASONAL_START & month(reference_date) %in% SEASON_MONTHS]

score_task <- function(j) {
  t <- tasks[j]; y <- t$series[[1]]
  td <- t$reference_date + 7L * H
  keep <- month(td) %in% SEASON_MONTHS                       # target week within October-April
  obs <- truth[.(t$location, td), value]
  keep <- keep & !is.na(obs)
  if (!any(keep)) return(NULL)
  set.seed(task_seed(t$target, t$location, t$reference_date))
  out <- list()
  for (i in seq_len(nrow(TREND))) {
    g <- TREND[i]
    q <- forecast_quantiles(y, g$transformation, TRUE, g$window, keep = H, count_data = FALSE)
    out[[i]] <- data.table(model = g$model, horizon = H[keep], score_quantiles(q[keep, , drop = FALSE], obs[keep]))
  }
  train <- data.table(date = t$reference_date - 7L * rev(seq_along(y) - 1L), value = y)
  q <- seasonal_forecast(train, td)
  out[[length(out) + 1]] <- data.table(model = "seasonal", horizon = H[keep], score_quantiles(q[keep, , drop = FALSE], obs[keep]))
  for (i in seq_len(nrow(LOG_TREND))) {
    g <- LOG_TREND[i]
    set.seed(task_seed(paste(t$target, g$model), t$location, t$reference_date))
    q <- forecast_quantiles(y, g$transformation, TRUE, g$window, keep = H, count_data = FALSE)
    out[[length(out) + 1]] <- data.table(model = g$model, horizon = H[keep], score_quantiles(q[keep, , drop = FALSE], obs[keep]))
  }
  for (v in names(SEASONAL_3Y)) {
    set.seed(task_seed(paste(t$target, v), t$location, t$reference_date))
    q <- do.call(seasonal_forecast, c(list(train = train, target_dates = td), SEASONAL_3Y[[v]]))
    out[[length(out) + 1]] <- data.table(model = v, horizon = H[keep], score_quantiles(q[keep, , drop = FALSE], obs[keep]))
  }
  rbindlist(out)[, `:=`(location = t$location, reference_date = t$reference_date)][]
}

scores <- rbindlist(parallel::mclapply(seq_len(nrow(tasks)), score_task,
                                       mc.cores = as.integer(Sys.getenv("NCORES", N_CORES))))

# Summary by model and horizon; % improvement over the seasonal baseline (all data, and the most
# recent 3 years) on identical forecasts
s <- scores[, .(mean_wis = mean(wis), cov50 = mean(cov50), cov95 = mean(cov95), n_forecasts = .N),
            by = .(model, horizon)]
improvement_vs <- function(ref, col) {
  seas <- scores[model == ref, .(location, reference_date, horizon, wis_ref = wis)]
  imp <- seas[scores[model != ref], on = .(location, reference_date, horizon), nomatch = 0][
    , .(pct = pct_improve(mean(wis), mean(wis_ref))), by = .(model, horizon)]
  setnames(imp, "pct", col)
}
s <- improvement_vs("seasonal", "pct_improvement_vs_seasonal")[s, on = .(model, horizon)]
s <- improvement_vs("seasonal_last3y", "pct_improvement_vs_seasonal_last3y")[s, on = .(model, horizon)]
setorder(s, model, horizon)
fwrite(s, file.path(OUT_DIR, "tables", "wili_long_horizon_summary.csv"))
print(dcast(s, horizon + n_forecasts ~ model, value.var = "pct_improvement_vs_seasonal"))
message("Wrote wili_long_horizon_summary.csv to ", file.path(OUT_DIR, "tables"))
