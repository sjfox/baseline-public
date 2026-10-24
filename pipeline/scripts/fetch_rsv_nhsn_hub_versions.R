# RSV (NHSN) data exactly as the CDC RSV Forecast Hub had it for each forecast week.
#
# The hub's target-data/time-series.parquet stores one version per weekly data release (as_of),
# but the archived versions were later rewritten: in the current file every version reaches back
# to 2023-11-25, whereas during the 2025-26 season the hub's data started at 2024-11-09. So the
# "real-time" series cannot be read from the current file. This script downloads the file as it
# was in the hub's repository at each week's submission deadline (Wednesday 23:59 ET before the
# Saturday reference date), or, if the week's data release was late (no data through the week
# before the reference date by the deadline, e.g. the 2025-12-29 holiday release), the first
# version after the deadline that has it, as the hub's baseline did. It keeps the file's latest
# version, which is what the hub's baseline used
# (hubhelpr::generate_hub_baseline with as_of = "latest"). Files go to
# data/rsv_nhsn/hub_versions/time-series_<reference date>.parquet; the commits used are recorded in
# pipeline/data_manifest/rsv_nhsn_hub_versions.csv, and the script reports how often the last
# observation equals the posted RSVHub-baseline median.
#   Rscript pipeline/scripts/fetch_rsv_nhsn_hub_versions.R
source("pipeline/R/setup.R")
suppressPackageStartupMessages(library(dplyr))

REPO <- "CDCgov/rsv-forecast-hub"
PATH <- "target-data/time-series.parquet"
out_dir <- file.path(DATA_DIR, "rsv_nhsn", "hub_versions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
manifest_file <- file.path("pipeline", "data_manifest", "rsv_nhsn_hub_versions.csv")

refs <- seq(PERIODS$rsv_nhsn[1] + 7, PERIODS$rsv_nhsn[2] + 7, by = 7)   # hub reference dates (Saturdays)
deadline <- as.POSIXct(paste(refs - 3, "23:59:59"), tz = "America/New_York")

if (file.exists(manifest_file)) {
  manifest <- fread(manifest_file, colClasses = c(sha = "character"))
} else {
  # commits that changed the data file (GitHub API, newest first)
  commits <- rbindlist(lapply(1:3, function(page) {
    url <- sprintf("https://api.github.com/repos/%s/commits?path=%s&since=%s&until=%s&per_page=100&page=%d",
                   REPO, PATH, "2025-09-01T00:00:00Z", "2026-07-01T00:00:00Z", page)
    x <- jsonlite::fromJSON(url)
    if (!length(x)) return(NULL)
    data.table(sha = x$sha, time = as.POSIXct(x$commit$committer$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }))
  last_date_at <- function(sha) {           # last date of the latest version in the file at a commit
    f <- tempfile(fileext = ".parquet")
    download.file(sprintf("https://raw.githubusercontent.com/%s/%s/%s", REPO, sha, PATH), f, mode = "wb", quiet = TRUE)
    max(read_rsv_nhsn_hub_version(f)$date)
  }
  setorder(commits, time)
  manifest <- rbindlist(lapply(seq_along(refs), function(i) {
    j <- max(which(commits$time <= deadline[i]))
    while (last_date_at(commits$sha[j]) < refs[i] - 7 && j < nrow(commits)) j <- j + 1L
    data.table(reference_date = refs[i], sha = commits$sha[j], commit_time_utc = format(commits$time[j], "%Y-%m-%d %H:%M"))
  }))
}

for (i in seq_len(nrow(manifest))) {
  f <- file.path(out_dir, sprintf("time-series_%s.parquet", manifest$reference_date[i]))
  if (!file.exists(f))
    download.file(sprintf("https://raw.githubusercontent.com/%s/%s/%s", REPO, manifest$sha[i], PATH), f,
                  mode = "wb", quiet = TRUE)
}

# Latest version in each file, the series the hub forecast from
manifest[, c("as_of", "first_date", "last_date") := {
  r <- lapply(reference_date, function(rd) {
    v <- read_rsv_nhsn_hub_version(file.path(out_dir, sprintf("time-series_%s.parquet", rd)))
    list(max(v$as_of), min(v$date), max(v$date))
  })
  list(as.Date(sapply(r, `[[`, 1)), as.Date(sapply(r, `[[`, 2)), as.Date(sapply(r, `[[`, 3)))
}]
fwrite(manifest, manifest_file)

# Check: the posted RSVHub-baseline median is the last observation of the hub's series
bl <- as.data.table(arrow::read_parquet(file.path(DATA_DIR, "hubs", "rsv_nhsn", "forecasts.parquet")))[
  model == HUB_BASELINE[["rsv_nhsn"]] & horizon == 0 & round(quantile, 3) == 0.5,
  .(reference_date = as.Date(reference_date), location, median = value)]
chk <- rbindlist(lapply(manifest$reference_date, function(rd) {
  v <- read_rsv_nhsn_hub_version(file.path(out_dir, sprintf("time-series_%s.parquet", rd)))
  last <- v[!is.na(value)][order(date)][, .(last = value[.N], last_date = date[.N]), by = location]
  merge(bl[reference_date == rd], last, by = "location")
}))
message(sprintf("rsv_nhsn hub versions: %d weeks (data through %s to %s); RSVHub-baseline median = last observation in %d of %d location-weeks",
                nrow(manifest), min(manifest$last_date), max(manifest$last_date), chk[median == last, .N], nrow(chk)))
