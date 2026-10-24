# Data loading and construction of forecast tasks.
#
# A "task" is one (target, location, forecast origin) with the series available at that
# origin. Each target returns:
#   tasks : data.table(task_id, target, snapshot, location, reference_date, series = list(y))
#   truth : data.table(location, date, value)  -- final data used for scoring
#   step  : 1 (daily) or 7 (weekly)
#
# Rules applied to every target:
#   * each location's series is placed on a complete date grid (gaps -> NA)
#   * trailing NAs are dropped; the forecast origin (reference_date) is the last observation
#   * each (location, reference_date) is forecast once; when several archived snapshots share
#     the same last observation, the earliest of them is used

library(data.table)

locations_tbl <- function() {
  fread(file.path(DATA_DIR, "locations.csv"), colClasses = "character")
}

# Put one location's observations on a complete grid and cut at the last observation.
complete_series <- function(dates, values, step) {
  o <- order(dates); dates <- dates[o]; values <- values[o]
  keep <- !is.na(values)
  if (!any(keep)) return(NULL)
  first <- min(dates[keep]); last <- max(dates[keep])
  grid <- seq(first, last, by = step)
  y <- values[match(grid, dates)]
  list(dates = grid, y = y)
}

# Build tasks from a long table of snapshots: columns snapshot, location, date, value.
tasks_from_snapshots <- function(snap, step, target) {
  setDT(snap)
  snap <- snap[!is.na(location)]
  res <- snap[, {
    s <- complete_series(date, value, step)
    if (is.null(s)) NULL else
      list(reference_date = max(s$dates), series = list(s$y), n_obs = sum(!is.na(s$y)))
  }, by = .(snapshot, location)]
  # each forecast origin once, from the earliest snapshot that contains it
  setorder(res, location, reference_date, snapshot)
  res <- res[, .SD[1], by = .(location, reference_date)]
  res[, target := target]
  res[]
}

# ---- COVID-19 (daily, archived snapshots) -------------------------------------------
load_covid <- function() {
  locs  <- locations_tbl()
  files <- sort(list.files(file.path(DATA_DIR, "covid", COVID_SNAPSHOT_DIR),
                           pattern = "^archived_target_data_\\d{4}-\\d{2}-\\d{2}\\.csv$",
                           full.names = TRUE))
  snap <- rbindlist(lapply(files, function(f) {
    d <- fread(f, colClasses = c(geo_value = "character", location = "character"))
    d[, .(snapshot = as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", f)),
          location = locs$location[match(toupper(geo_value), locs$abbreviation)],
          date = as.Date(time_value), value = as.numeric(value))]
  }))
  tasks <- tasks_from_snapshots(snap, step = 1, target = "covid")
  tasks <- tasks[between(snapshot, PERIODS$covid[1], PERIODS$covid[2])]
  tr <- fread(file.path(DATA_DIR, "covid", "truth_covid_daily.csv"),
              colClasses = c(geo_value = "character", location = "character"))
  truth <- tr[, .(location = locs$location[match(toupper(geo_value), locs$abbreviation)],
                  date = as.Date(time_value), value = as.numeric(value))][!is.na(location)]
  list(tasks = tasks, truth = truth, step = 1, count_data = TRUE)
}

# ---- Influenza hospital admissions (weekly, archived snapshots) ----------------------
# Each FluSight forecast week's data as the hub served it at the submission deadline
# (scripts/fetch_flu_hub_versions.R), filed under the hub reference date. The snapshot date is the
# week the data run through (reference date - 7).
read_flu_snapshots <- function() {
  files <- sort(list.files(file.path(DATA_DIR, "Influenza", "hub_versions"),
                           pattern = "^target-hospital-admissions_\\d{4}-\\d{2}-\\d{2}\\.csv$",
                           full.names = TRUE))
  if (!length(files)) stop("run pipeline/scripts/fetch_flu_hub_versions.R first")
  rbindlist(lapply(files, function(f) {
    d <- fread(f, colClasses = c(location = "character"))
    d[, .(snapshot = as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", f)) - 7,
          location, date = as.Date(date), value = as.numeric(value))]
  }))
}

load_flu <- function() {
  snap <- read_flu_snapshots()
  tasks <- tasks_from_snapshots(snap, step = 7, target = "flu")
  tasks <- tasks[in_flu_seasons(reference_date)]
  tr <- fread(file.path(DATA_DIR, "Influenza", "archive", "truth_flu.csv"),
              colClasses = c(location = "character"))
  truth <- tr[, .(location, date = as.Date(date), value = as.numeric(value))]
  list(tasks = tasks, truth = truth, step = 7, count_data = TRUE)
}

# ---- RSV truth (finalised RSV-NET, all ages, counts) ---------------------------------
rsv_truth <- function() {
  tr <- fread(file.path(DATA_DIR, "rsv", "truth_rsv.csv"), colClasses = c(location = "character"))
  tr[age_group == "0-130" & target == "inc hosp",
     .(location, date = as.Date(date), value = round(as.numeric(value)))]
}

# Tasks for "final data" targets: truncate the final series at every candidate origin.
# A location is forecast at an origin only if it has an observation on that date.
tasks_from_final <- function(truth, origins, step, target) {
  rbindlist(lapply(split(truth, by = "location"), function(d) {
    s <- complete_series(d$date, d$value, step)
    if (is.null(s)) return(NULL)
    org <- origins[origins %in% s$dates]
    rbindlist(lapply(org, function(o) {
      i <- match(o, s$dates)
      if (is.na(s$y[i])) return(NULL)
      y <- s$y[seq_len(i)]
      data.table(snapshot = o, location = d$location[1], reference_date = o,
                 series = list(y), n_obs = sum(!is.na(y)))
    }))
  }))[, target := target][]
}

# Final-data counterpart of a real-time target: the final series cut off at exactly the same
# forecast dates and locations, and starting no earlier than the real-time release, so the two
# differ only in revisions (the NHSN hub releases start later than the latest version).
final_like <- function(rt, target) {
  keys <- unique(rt$tasks[, .(location, reference_date)])
  tasks <- tasks_from_final(rt$truth, sort(unique(keys$reference_date)), step = rt$step, target = target)
  tasks <- tasks[keys, on = .(location, reference_date), nomatch = 0]
  len_rt <- rt$tasks[tasks, on = .(location, reference_date), lengths(series)]
  tasks[, series := Map(function(y, n) utils::tail(y, n), series, len_rt)]
  tasks[, n_obs := vapply(series, function(y) sum(!is.na(y)), integer(1))]
  rt$tasks <- tasks
  rt
}

# ---- RSV, finalised data --------------------------------------------------------------
load_rsv_final <- function() final_like(load_rsv_asof(), "rsv")

# ---- RSV, archived real-time snapshots ------------------------------------------------
read_rsv_snapshots <- function() {
  files <- sort(list.files(file.path(DATA_DIR, "rsv", "archive"),
                           pattern = "_rsvnet_hospitalization\\.csv$", full.names = TRUE))
  snap <- rbindlist(lapply(files, function(f) {
    d <- fread(f, colClasses = c(location = "character"))
    d[age_group == "0-130" & target == "inc hosp",
      .(snapshot = as.Date(substr(basename(f), 1, 10)), location,
        date = as.Date(date), value = round(as.numeric(value)))]
  }))
  # A snapshot cannot contain data dated after its own issue date; such files are excluded.
  bad <- snap[, .(maxdate = max(date)), by = snapshot][maxdate > snapshot]
  if (nrow(bad)) {
    message("Dropping RSV snapshots with data after issue date: ",
            paste(bad$snapshot, collapse = ", "))
    snap <- snap[!snapshot %in% bad$snapshot]
  }
  snap
}

load_rsv_asof <- function() {
  snap <- read_rsv_snapshots()
  tasks <- tasks_from_snapshots(snap, step = 7, target = "rsv_asof")
  list(tasks = tasks, truth = rsv_truth(), step = 7, count_data = TRUE)
}

# ---- RSV, NHSN hospital admissions (CDC RSV Forecast Hub; main RSV analysis) ------------
# Each weekly version (as_of, the Wednesday release) of the hub's target data is a snapshot; the
# forecast origin is its last week. Kept: CDC RSV Forecast Hub 2025-26 forecast weeks (hub reference
# date = origin + 7). Truth: the latest version.
# One version of the CDC RSV Forecast Hub's target data as downloaded from its repository history
# (scripts/fetch_rsv_nhsn_hub_versions.R): the latest data release in that file, as the hub used it.
read_rsv_nhsn_hub_version <- function(f) {
  d <- as.data.table(arrow::read_parquet(f))
  if (!"target_end_date" %in% names(d)) setnames(d, "date", "target_end_date")   # column renamed in 2026
  d <- d[target == "wk inc rsv hosp"]
  d[as_of == max(as_of), .(as_of = as.Date(as_of), location = as.character(location),
                           date = as.Date(target_end_date), value = as.numeric(observation))]
}

read_rsv_nhsn <- function() {
  d <- as.data.table(arrow::read_parquet(file.path(DATA_DIR, "rsv_nhsn", "time-series.parquet")))
  d[target == "wk inc rsv hosp", .(as_of = as.Date(as_of), location = as.character(location),
                                   date = as.Date(target_end_date), value = as.numeric(observation))]
}

load_rsv_nhsn <- function() {
  d <- read_rsv_nhsn()   # current hub file: final data for scoring
  # Forecast tasks: each week's data exactly as the hub had it (its archived versions were later
  # extended back in time, so the current file's versions are not what was available)
  files <- sort(list.files(file.path(DATA_DIR, "rsv_nhsn", "hub_versions"),
                           pattern = "^time-series_\\d{4}-\\d{2}-\\d{2}\\.parquet$", full.names = TRUE))
  if (!length(files)) stop("run pipeline/scripts/fetch_rsv_nhsn_hub_versions.R first")
  snap <- rbindlist(lapply(files, read_rsv_nhsn_hub_version))[, .(snapshot = as_of, location, date, value)]
  tasks <- tasks_from_snapshots(snap, step = 7, target = "rsv_nhsn")
  tasks <- tasks[between(reference_date, PERIODS$rsv_nhsn[1], PERIODS$rsv_nhsn[2])]
  truth <- d[as_of == max(as_of), .(location, date, value)]
  list(tasks = tasks, truth = truth, step = 7, count_data = TRUE)
}

# ---- RSV, NHSN finalised data (latest version, same forecast dates as rsv_nhsn) ----------
load_rsv_nhsn_final <- function() final_like(load_rsv_nhsn(), "rsv_nhsn_final")

# ---- wILI (weekly, from the start of year-round reporting) ----------------------------
# Final wILI is used only as the truth for scoring and as the history older than each issue
# window. All wILI models (trend and seasonal) use history from ILI_HISTORY_START.
ili_truth_full <- function() {
  tr <- fread(file.path(DATA_DIR, "ili", "truth_ili_seasonal.csv"))
  tr[, .(location, date = as.Date(target_end_date), value = as.numeric(value))][date >= ILI_HISTORY_START]
}

# ---- wILI, versioned (as-of) data ------------------------------------------------------
# Each issue = final values for weeks older than the issue's 105-week window (revisions
# beyond two years are negligible) + the values as reported in that issue for recent weeks.
load_ili_asof <- function() {
  final <- ili_truth_full()
  v <- fread(file.path(DATA_DIR, "ili", "archive", "ili_versions.csv"))
  v[, `:=`(epiweek_start = as.Date(epiweek_start), snapshot = as.Date(NA))]
  iss_dates <- v[, .(snapshot = max(epiweek_start), first_v = min(epiweek_start)), by = issue_ew]
  snap <- rbindlist(lapply(seq_len(nrow(iss_dates)), function(i) {
    r <- iss_dates[i]
    rbind(final[date < r$first_v & date <= r$snapshot, .(snapshot = r$snapshot, location, date, value)],
          v[issue_ew == r$issue_ew, .(snapshot = r$snapshot, location = region,
                                      date = epiweek_start, value = wili)])
  }))
  tasks <- tasks_from_snapshots(snap, step = 7, target = "ili_asof")
  tasks <- tasks[reference_date >= SEASONAL_START]
  list(tasks = tasks, truth = final, step = 7, count_data = FALSE)
}

MIN_OBS <- 10                 # minimum number of observations needed to forecast
RSV_EXCLUDE <- "37"           # North Carolina joins RSV-NET in Oct 2024 with no history

# Build a target's forecast tasks and truth from the raw downloads in data/. Only
# 01_build_data.R calls this; it writes the result to the vintaged data files below.
build_target <- function(target) {
  d <- build_target_raw(target)
  d$tasks <- d$tasks[n_obs >= MIN_OBS]
  if (target %in% c("rsv", "rsv_asof")) d$tasks <- d$tasks[!location %in% RSV_EXCLUDE]
  d
}

build_target_raw <- function(target) {
  switch(target,
         covid = load_covid(), flu = load_flu(), rsv = load_rsv_final(),
         rsv_asof = load_rsv_asof(), rsv_nhsn = load_rsv_nhsn(), rsv_nhsn_final = load_rsv_nhsn_final(),
         ili_asof = load_ili_asof(),
         stop("unknown target ", target))
}

# ---- Vintaged data files (output/vintaged/) -----------------------------------------
# One pair of files per target, written by 01_build_data.R and read by every later step:
#   <target>_vintages.parquet: one row per (forecast task, date): snapshot (release used),
#     location, reference_date (forecast origin = last observation), date, value. The rows of a
#     task are its whole series as the model sees it, on a complete weekly or daily grid (gaps NA).
#   <target>_truth.parquet: location, date, value (the data forecasts are scored against).
#   targets.csv: one row per target: step (1 = daily, 7 = weekly), count_data, and a summary.
VINTAGED_DIR <- file.path(OUT_DIR, "vintaged")

write_target <- function(target) {
  d <- build_target(target)
  long <- d$tasks[, .(date = reference_date - d$step * (rev(seq_along(series[[1]])) - 1L),
                      value = series[[1]]), by = .(snapshot, location, reference_date)]
  setorder(long, location, reference_date, date)
  dir.create(VINTAGED_DIR, showWarnings = FALSE)
  arrow::write_parquet(long, file.path(VINTAGED_DIR, paste0(target, "_vintages.parquet")))
  arrow::write_parquet(d$truth[order(location, date)], file.path(VINTAGED_DIR, paste0(target, "_truth.parquet")))
  invisible(d)
}

load_target <- function(target) {
  meta <- fread(file.path(VINTAGED_DIR, "targets.csv"))
  i <- match(target, meta$target)
  if (is.na(i)) stop("no vintaged data for ", target, "; run pipeline/scripts/01_build_data.R first")
  meta <- meta[i]
  long <- as.data.table(arrow::read_parquet(file.path(VINTAGED_DIR, paste0(target, "_vintages.parquet"))))
  setorder(long, location, reference_date, date)
  tasks <- long[, .(series = list(value), n_obs = sum(!is.na(value))), by = .(location, reference_date, snapshot)]
  tasks[, target := target]
  truth <- as.data.table(arrow::read_parquet(file.path(VINTAGED_DIR, paste0(target, "_truth.parquet"))))
  list(tasks = tasks[], truth = truth, step = meta$step, count_data = meta$count_data)
}
