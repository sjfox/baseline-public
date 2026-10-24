# COVID-19 snapshots on the data vintage the COVID-19 Forecast Hub used for each forecast week.
#
# The HealthData.gov release published on a forecast Monday was usually not yet in the hub's data
# when that week's forecasts were made. The hub's own baseline (COVIDhub-baseline, a flatline whose
# median is the last observed value) shows which data it had: for each hub week this script finds
# the daily release (data/covid/archive_daily/, one covidData issue per file, from the 7 days up to
# the Monday) that reproduces the COVIDhub-baseline median in the most locations, allowing the
# hub's series for a location to end up to MAX_DROP days before the release's last date (the hub's
# data for some states ended a day or two earlier). Ties go to the earliest such release, so no
# revision later than the hub's data is used. Each location's series is that release, cut where
# the hub's series ended (or the full release where no cut reproduces the median). The result is
# written to data/covid/archive_hub_vintage/ under the hub week's Monday date, which load_covid()
# reads; the choices are recorded in pipeline/data_manifest/covid_hub_vintage.csv (one row per
# week) and covid_hub_vintage_locations.csv (days dropped per location and week).
#   Rscript pipeline/scripts/covid_hub_vintage.R
source("pipeline/R/setup.R")
suppressPackageStartupMessages(library(dplyr))

daily_dir <- file.path(DATA_DIR, "covid", "archive_daily")
out_dir   <- file.path(DATA_DIR, "covid", COVID_SNAPSHOT_DIR)
dir.create(out_dir, showWarnings = FALSE)
locs <- locations_tbl()

issues <- sort(as.Date(sub("^archived_target_data_(.*)\\.csv$", "\\1",
                           list.files(daily_dir, pattern = "^archived_target_data_.*\\.csv$"))))
read_issue <- function(d) {
  x <- fread(file.path(daily_dir, sprintf("archived_target_data_%s.csv", d)),
             colClasses = c(geo_value = "character", location = "character"))
  x[, location := locs$location[match(toupper(geo_value), locs$abbreviation)]]
  x[!is.na(value) & !is.na(location)]
}
MAX_DROP <- 3L
# the last MAX_DROP + 1 values of each location's series in a release (k = days before its end)
tail_values <- function(d) {
  x <- read_issue(d)[, k := as.integer(max(as.Date(time_value)) - as.Date(time_value)), by = location]
  x[k <= MAX_DROP, .(location, k, value)]
}

# COVIDhub-baseline median for each hub week (Monday on or after the forecast date) and location
hub_week <- function(d) d + (8L - as.integer(format(d, "%u"))) %% 7L
bl <- as.data.table(arrow::open_dataset(file.path(DATA_DIR, "hubs", "covid", "forecasts.parquet")) |>
  filter(model == HUB_BASELINE[["covid"]], quantile == 0.5) |>
  select(forecast_date, location, horizon, value) |> collect())
bl[, week := hub_week(as.Date(forecast_date))]
bl <- bl[week >= PERIODS$covid[1] & week <= PERIODS$covid[2]]
bl <- bl[order(-as.integer(forecast_date), horizon)][, .(median = value[1]), by = .(week, location)]

weeks <- sort(unique(bl$week))
choice <- lapply(weeks, function(w) {
  cand <- issues[issues <= w & issues >= w - 7]
  per_issue <- rbindlist(lapply(cand, function(d) {
    x <- merge(bl[week == w], tail_values(d), by = "location")[median == value]
    x[order(k)][, .SD[1], by = location][, .(issue = d, location, k)]
  }))
  n_match <- per_issue[, .N, by = issue]
  best <- n_match[N == max(N)][which.min(issue), issue]
  list(week = data.table(snapshot = w, issue = best, days_before = as.integer(w - best),
                         n_locations = uniqueN(bl[week == w]$location), n_matched = max(n_match$N),
                         n_cut = per_issue[issue == best & k > 0, .N]),
       locations = per_issue[issue == best, .(snapshot = w, location, days_dropped = k)])
})
manifest <- rbindlist(lapply(choice, `[[`, "week"))
loc_manifest <- rbindlist(lapply(choice, `[[`, "locations"))

for (i in seq_len(nrow(manifest))) {
  w <- manifest$snapshot[i]
  x <- fread(file.path(daily_dir, sprintf("archived_target_data_%s.csv", manifest$issue[i])),
             colClasses = c(geo_value = "character", location = "character"))
  x[, loc := locs$location[match(toupper(geo_value), locs$abbreviation)]]
  x[, last_date := max(as.Date(time_value)[!is.na(value)]), by = geo_value]
  x <- loc_manifest[snapshot == w, .(loc = location, days_dropped)][x, on = "loc"]
  x <- x[is.na(days_dropped) | as.Date(time_value) <= last_date - days_dropped]
  x[, c("loc", "last_date", "days_dropped") := NULL]
  fwrite(x, file.path(out_dir, sprintf("archived_target_data_%s.csv", w)), na = "NA")
}
fwrite(manifest, file.path("pipeline", "data_manifest", "covid_hub_vintage.csv"))
fwrite(loc_manifest, file.path("pipeline", "data_manifest", "covid_hub_vintage_locations.csv"))
message(sprintf(paste("covid hub vintage: %d weeks; COVIDhub-baseline median reproduced in %d of %d",
                      "location-weeks (%d with the last 1-%d days dropped); release days before the Monday: %s"),
                nrow(manifest), sum(manifest$n_matched), sum(manifest$n_locations), sum(manifest$n_cut), MAX_DROP,
                paste(sprintf("%s (%d wk)", names(table(manifest$days_before)), table(manifest$days_before)), collapse = ", ")))
