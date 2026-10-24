# Score real-time hub submissions on the same forecast cells as our baselines.
# Hub data: data/hubs/<hub>/forecasts.parquet (scripts/fetch_hub_forecasts.R, scripts/fetch_rsv_nhsn.R).
# Each hub forecast is matched to one of our forecast cells
# (location, our origin, our horizon 1-4) by target end date and forecast week:
#   covid: hub forecast week = our snapshot. Our snapshot is the hub's Monday issue; a hub
#          forecast_date belongs to the week of the Monday on or after it (Sunday submissions count
#          for the next day's Monday round). target date = origin + 7k. When the data lag the
#          snapshot by more than 7 days (Jun-Oct 2023: last observation = snapshot - 10), our
#          horizon-1 target is before the hub round, so hub models have no forecast for it.
#   flu  : FluSight reference date = our origin + 7 (data through our origin); target = origin + 7k
#   rsv_nhsn: CDC RSV hub reference date = our origin + 7 (as FluSight); target = origin + 7k
# Scored against the same final truth with the same scorer.
suppressPackageStartupMessages(library(dplyr))

hub_map <- c(covid = "covid", flu = "flu", rsv_nhsn = "rsv_nhsn")
# Monday on or after each date (the COVID-19 Forecast Hub's weekly round a submission belongs to)
hub_week <- function(d) d + (8L - as.integer(format(d, "%u"))) %% 7L

score_hub <- function(target) {
  hub <- hub_map[[target]]
  dat <- load_target(target)
  step <- dat$step
  steps <- if (step == 1) HORIZONS_DAILY else HORIZONS_WEEKLY
  cells <- dat$tasks[, .(location, reference_date, snapshot)]
  cells <- cells[, .(horizon = seq_along(steps), target_end_date = reference_date + steps * step),
                 by = .(location, reference_date, snapshot)]
  truth <- dat$truth[!is.na(value)]; setkey(truth, location, date)

  ds <- arrow::open_dataset(file.path(DATA_DIR, "hubs", hub, "forecasts.parquet"))
  tdates <- sort(unique(cells$target_end_date))
  chunks <- split(tdates, ceiling(seq_along(tdates) / 15))
  qcols <- as.character(QUANTILES)
  out <- rbindlist(lapply(chunks, function(td) {
    h <- as.data.table(ds |> filter(target_end_date %in% td) |>
                         select(model, forecast_date, reference_date, target_end_date,
                                location, quantile, value) |> collect())
    if (!nrow(h)) return(NULL)
    setnames(h, "reference_date", "hub_reference_date")
    h[, quantile := round(quantile, 3)]
    m <- merge(h, cells[target_end_date %in% td], by = c("location", "target_end_date"),
               allow.cartesian = TRUE)
    m <- switch(target,
      covid    = m[hub_week(forecast_date) == snapshot],
      flu      = m[hub_reference_date == reference_date + 7],
      rsv_nhsn = m[hub_reference_date == reference_date + 7])
    if (!nrow(m)) return(NULL)
    # a model with >1 submission for the same cell (e.g. Sunday and Monday): keep the latest
    m <- m[m[, .I[forecast_date == max(forecast_date)], by = .(model, location, reference_date, horizon)]$V1]
    w <- dcast(m, model + location + reference_date + horizon + target_end_date ~ quantile,
               value.var = "value", fun.aggregate = function(x) x[length(x)], fill = NA)
    miss <- setdiff(qcols, names(w)); if (length(miss)) w[, (miss) := NA_real_]
    w <- w[complete.cases(w[, ..qcols])]
    obs <- truth[.(w$location, w$target_end_date), value]
    w[, observed := obs]
    w <- w[!is.na(observed)]
    if (!nrow(w)) return(NULL)
    q <- t(apply(as.matrix(w[, ..qcols]), 1, sort))     # guard against crossing quantiles
    cbind(w[, .(model, location, reference_date, horizon, target_end_date, observed)],
          score_quantiles(q, w$observed), wis_log = wis_log_scale(q, w$observed))
  }))
  out[, target := target]
  arrow::write_parquet(out, file.path(OUT_DIR, "scores", paste0("hub_", target, ".parquet")))
  message(sprintf("[hub %s] %d scored forecasts from %d models", target, nrow(out), uniqueN(out$model)))
  out
}
