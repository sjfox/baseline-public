# Influenza hospital admissions exactly as FluSight had them for each forecast week.
#
# Downloads FluSight's target-data/target-hospital-admissions.csv as it was in the hub's repository at
# each week's submission deadline (Wednesday 23:59 ET before the Saturday reference date) or, if the week's
# data release was late (no data through the week before the reference date by the deadline,
# e.g. holiday weeks), the first version after the deadline that has it. Reference dates are all
# FluSight forecast weeks in FLU_SEASONS; a week whose data never arrived in time is dropped. Files go to
# data/Influenza/hub_versions/target-hospital-admissions_<reference date>.csv; the commits used are
# recorded in pipeline/data_manifest/flu_hub_versions.csv, with how often the last observation
# equals the posted FluSight-baseline median.
#   Rscript pipeline/scripts/fetch_flu_hub_versions.R
source("pipeline/R/setup.R")
suppressPackageStartupMessages(library(dplyr))

REPO <- "cdcepi/FluSight-forecast-hub"
PATH <- "target-data/target-hospital-admissions.csv"
out_dir <- file.path(DATA_DIR, "Influenza", "hub_versions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
manifest_file <- file.path("pipeline", "data_manifest", "flu_hub_versions.csv")

refs <- sort(unlist(lapply(FLU_SEASONS, function(w) seq(w[1], w[2], by = 7)))) + 7   # origin + 7
refs <- as.Date(refs, origin = "1970-01-01")
deadline <- as.POSIXct(paste(refs - 3, "23:59:59"), tz = "America/New_York")
read_version <- function(f) {
  d <- fread(f, colClasses = c(location = "character"))
  d[, .(location, date = as.Date(date), value = as.numeric(value))]
}
raw_url <- function(sha) sprintf("https://raw.githubusercontent.com/%s/%s/%s", REPO, sha, PATH)

if (file.exists(manifest_file)) {
  manifest <- fread(manifest_file, colClasses = c(sha = "character"))[, .(reference_date = as.Date(reference_date), sha, commit_time_utc)]
} else {
  commits <- rbindlist(lapply(1:5, function(page) {
    url <- sprintf("https://api.github.com/repos/%s/commits?path=%s&since=%s&until=%s&per_page=100&page=%d",
                   REPO, PATH, "2023-09-01T00:00:00Z", "2025-07-01T00:00:00Z", page)
    x <- jsonlite::fromJSON(url)
    if (!length(x)) return(NULL)
    data.table(sha = x$sha, time = as.POSIXct(x$commit$committer$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }))
  setorder(commits, time)
  last_date_at <- function(sha) {
    f <- tempfile(fileext = ".csv"); download.file(raw_url(sha), f, quiet = TRUE)
    max(read_version(f)[!is.na(value), date])
  }
  manifest <- rbindlist(lapply(seq_along(refs), function(i) {
    j <- max(which(commits$time <= deadline[i]))
    while (last_date_at(commits$sha[j]) < refs[i] - 7 && j < nrow(commits)) j <- j + 1L
    data.table(reference_date = refs[i], sha = commits$sha[j], commit_time_utc = format(commits$time[j], "%Y-%m-%d %H:%M"))
  }))
}

for (i in seq_len(nrow(manifest))) {
  f <- file.path(out_dir, sprintf("target-hospital-admissions_%s.csv", manifest$reference_date[i]))
  if (!file.exists(f)) download.file(raw_url(manifest$sha[i]), f, quiet = TRUE)
}

# Check against the posted FluSight-baseline (median = last observation in the hub's data)
bl <- as.data.table(arrow::read_parquet(file.path(DATA_DIR, "hubs", "flu", "forecasts.parquet")))[
  model == HUB_BASELINE[["flu"]] & horizon == 0 & round(quantile, 3) == 0.5,
  .(reference_date = as.Date(reference_date), location, median = value)]
chk <- rbindlist(lapply(seq_len(nrow(manifest)), function(i) {
  rd <- manifest$reference_date[i]
  v <- read_version(file.path(out_dir, sprintf("target-hospital-admissions_%s.csv", rd)))[!is.na(value)]
  last <- v[order(date)][, .(last = value[.N]), by = location]
  m <- merge(bl[reference_date == rd], last, by = "location")
  data.table(reference_date = rd, last_date = max(v$date), n_baseline = nrow(m),
             baseline_match_rate = if (nrow(m)) round(mean(abs(m$median - m$last) < 0.5), 4) else NA_real_)
}))
manifest <- merge(manifest, chk, by = "reference_date")
# A week whose data arrived only after the next week's release (no version with data through
# exactly reference date - 7) had no real-time forecast round (e.g. 2025-01-25, during the January
# 2025 federal communications pause, when FluSight posted no baseline): dropped.
skipped <- manifest[last_date != reference_date - 7]
for (rd in skipped$reference_date) unlink(file.path(out_dir, sprintf("target-hospital-admissions_%s.csv", as.Date(rd))))
if (nrow(skipped)) message("flu hub versions: dropped weeks without data through the week before the reference date: ",
                           paste(skipped$reference_date, collapse = ", "))
manifest <- manifest[last_date == reference_date - 7]
fwrite(manifest, manifest_file)
message(sprintf("flu hub versions: %d weeks (data through %s to %s); FluSight-baseline median = last observation in all locations in %d of %d weeks with a baseline; weeks below 100%%: %s",
                nrow(manifest), min(manifest$last_date), max(manifest$last_date),
                manifest[baseline_match_rate == 1, .N], manifest[n_baseline > 0, .N],
                paste(manifest[baseline_match_rate < 1, sprintf("%s (%.0f%%)", reference_date, 100 * baseline_match_rate)], collapse = ", ")))
