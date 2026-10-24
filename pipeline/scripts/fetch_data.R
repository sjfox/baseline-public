# Download the surveillance data used by the pipeline into data/.
# Usage (from the project root):
#   Rscript pipeline/scripts/fetch_data.R [locations] [covid] [flu] [rsv] [ili]
# Set DATA_OUT_DIR to write somewhere other than data/.
#
# Sources
#   locations  COVID-19 Forecast Hub location list (pinned commit), state level
#   covid      Daily COVID-19 hospital admissions as released on each day, rebuilt from the HealthData.gov
#              revision history with the covidData package (github.com/reichlab/covidData), plus the version
#              of 2025-07-03 as final data. Release dates are listed in
#              pipeline/data_manifest/covid_daily_issue_dates.txt. The step is resumable.
#   flu        FluSight final target data (pinned commit)
#   rsv        RSV Forecast Hub RSV-NET archive (weekly snapshots listed in pipeline/data_manifest/) and
#              the final RSV-NET data as of 2025-11-14 (pinned commit)
#   ili        Final wILI for the 10 HHS regions and national, 1997/98 onwards, from the Delphi Epidata
#              API (fluview). Versioned wILI is downloaded by fetch_ili_versions.R.
source("pipeline/R/setup.R")

OUT <- Sys.getenv("DATA_OUT_DIR", DATA_DIR)
raw_url <- function(repo, sha, path) sprintf("https://raw.githubusercontent.com/%s/%s/%s", repo, sha, path)
download <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest)) return(invisible(dest))
  tmp <- paste0(dest, ".part")
  if (utils::download.file(url, tmp, quiet = TRUE, mode = "wb") != 0) stop("download failed: ", url)
  file.rename(tmp, dest)
}

COVID_HUB <- list(repo = "reichlab/covid19-forecast-hub", sha = "3a6997b6abdcfabe321dd059ebe32334095d49b5")
COVID_FINAL_ISSUE <- as.Date("2025-07-03")   # version of the HealthData.gov series used as final data
FLUSIGHT  <- list(repo = "cdcepi/FluSight-forecast-hub",
                  final_sha = "35d835070746230ca87ff7116aa92f6da3c634b8")   # target data through 2025-06-07
RSV_HUB   <- list(repo = "HopkinsIDD/rsv-forecast-hub", sha = "6bf251ac3fc798e55ed95d0b7269f5f5781c0207",
                  final_file = "2025-11-14_rsvnet_hospitalization.csv")

# ---- Locations ---------------------------------------------------------------------------
fetch_locations <- function() {
  x <- fread(raw_url(COVID_HUB$repo, COVID_HUB$sha, "data-locations/locations.csv"), colClasses = "character")
  # States, DC, PR, VI and national (excludes counties and the smaller territories)
  x <- x[(nchar(location) == 2 | location == "US") & !location %in% c("60", "66", "69", "74")]
  fwrite(x, file.path(OUT, "locations.csv"))
  message("locations: ", nrow(x), " rows")
}

# ---- COVID-19 ----------------------------------------------------------------------------
# Releases are kept as published, last date included (drop_last_date = FALSE). Until June 2023 a
# location's last value usually repeated the day before; the repeat is in the HHS data itself.
# We keep it because that is what the hub had: COVIDhub-baseline loaded the same releases with
# covidData's default drop_last_date = FALSE and had no rule for repeated values. Dropping the last
# date would test a data-cleaning step the hub never applied, so it is not analysed.
covid_snapshot <- function(issue, locs) {
  x <- as.data.table(covidData::load_data(
    issues = issue, spatial_resolution = c("state", "national"), temporal_resolution = "daily",
    measure = "hospitalizations", source = NULL, drop_last_date = FALSE))
  x[, .(geo_value = tolower(locs$abbreviation[match(location, locs$location)]),
        time_value = as.Date(date), value = inc, location, issue_date = as.Date(issue))][order(location, time_value)]
}

fetch_covid <- function(cores = 4) {
  locs <- fread(file.path(OUT, "locations.csv"), colClasses = "character")
  issues_used <- as.Date(readLines(file.path("pipeline", "data_manifest", "covid_daily_issue_dates.txt")))
  dir <- file.path(OUT, "covid", "archive_daily")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  todo <- issues_used[!file.exists(file.path(dir, sprintf("archived_target_data_%s.csv", as.Date(issues_used, origin = "1970-01-01"))))]
  message("covid: ", length(issues_used), " releases, ", length(todo), " to build")
  parallel::mclapply(todo, function(d) {
    d <- as.Date(d, origin = "1970-01-01")
    fwrite(covid_snapshot(d, locs), file.path(dir, sprintf("archived_target_data_%s.csv", d)), na = "NA")
    message("  built ", d)
  }, mc.cores = cores, mc.preschedule = FALSE)
  fwrite(covid_snapshot(COVID_FINAL_ISSUE, locs), file.path(OUT, "covid", "truth_covid_daily.csv"), na = "NA")
  message("covid final data: issue ", COVID_FINAL_ISSUE)
}

# ---- Influenza ---------------------------------------------------------------------------
fetch_flu <- function() {
  download(raw_url(FLUSIGHT$repo, FLUSIGHT$final_sha, "target-data/target-hospital-admissions.csv"),
           file.path(OUT, "Influenza", "archive", "truth_flu.csv"))
  message("flu: final data")
}

# ---- RSV ---------------------------------------------------------------------------------
fetch_rsv <- function() {
  files <- readLines(file.path("pipeline", "data_manifest", "rsv_archive_files.txt"))
  for (f in files) download(raw_url(RSV_HUB$repo, RSV_HUB$sha, file.path("target-data/archive", f)),
                            file.path(OUT, "rsv", "archive", f))
  download(raw_url(RSV_HUB$repo, RSV_HUB$sha, file.path("target-data/archive", RSV_HUB$final_file)),
           file.path(OUT, "rsv", "truth_rsv.csv"))
  message("rsv: ", length(files), " snapshots + final data")
}

# ---- wILI --------------------------------------------------------------------------------
fetch_ili <- function() {
  regions <- c("nat", paste0("hhs", 1:10))
  epiweek_ranges <- list(c(199740, 200952), c(201001, 202320))   # two requests per region
  x <- rbindlist(lapply(regions, function(r) {
    rbindlist(lapply(epiweek_ranges, function(e) {
      as.data.table(epidatr::pub_fluview(regions = r, epiweeks = epidatr::epirange(e[1], e[2])))
    }))
  }))
  out <- x[, .(target_end_date = as.Date(epiweek), location = region, value = wili,
               target_variable = "inc wili", model = "observed data")][order(target_end_date, location)]
  dir.create(file.path(OUT, "ili"), recursive = TRUE, showWarnings = FALSE)
  fwrite(out, file.path(OUT, "ili", "truth_ili_seasonal.csv"))
  message("ili: ", nrow(out), " rows (latest issue ", format(max(as.Date(x$issue))), ")")
}

args <- commandArgs(trailingOnly = TRUE)
for (src in if (length(args)) args else c("locations", "covid", "flu", "rsv", "ili"))
  switch(src, locations = fetch_locations(), covid = fetch_covid(), flu = fetch_flu(),
         rsv = fetch_rsv(), ili = fetch_ili(), stop("unknown source ", src))
