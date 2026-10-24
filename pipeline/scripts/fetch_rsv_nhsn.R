# Download the CDC RSV Forecast Hub data used for the main RSV analysis (NHSN hospital admissions).
#   data/rsv_nhsn/time-series.parquet : hub target data, every weekly version (as_of) of NHSN
#                                       `totalconfrsvnewadm` by state, DC, PR and US
#   data/hubs/rsv_nhsn/forecasts.parquet : quantile forecasts of `wk inc rsv hosp` from every model,
#                                       2025-26 season reference dates (same columns as the other hubs)
# Source: https://github.com/CDCgov/rsv-forecast-hub, pinned to the commit below.
# Run from the project root: Rscript pipeline/scripts/fetch_rsv_nhsn.R   (needs git)
suppressPackageStartupMessages({ library(data.table); library(arrow) })
REPO   <- "https://github.com/CDCgov/rsv-forecast-hub.git"
COMMIT <- "c8dc81bf625c1c4a6faa71714a2c03cd952572ed"   # main, 2026-10-15
SEASON <- as.Date(c("2025-09-01", "2026-06-30"))       # reference dates kept (2025-26 season)

tmp <- file.path(tempdir(), "rsv-forecast-hub")
unlink(tmp, recursive = TRUE)
run <- function(...) { st <- system2("git", c(...)); if (st != 0) stop("git failed: ", paste(...)) }
run("init", "-q", tmp)
run("-C", tmp, "remote", "add", "origin", REPO)
run("-C", tmp, "fetch", "-q", "--depth", "1", "origin", COMMIT)
run("-C", tmp, "checkout", "-q", "FETCH_HEAD")

dir.create("data/rsv_nhsn", recursive = TRUE, showWarnings = FALSE)
file.copy(file.path(tmp, "target-data", "time-series.parquet"), "data/rsv_nhsn/time-series.parquet", overwrite = TRUE)

files <- list.files(file.path(tmp, "model-output"), pattern = "\\.(csv|parquet)$", recursive = TRUE, full.names = TRUE)
ref <- as.Date(substr(basename(files), 1, 10))
files <- files[!is.na(ref) & ref >= SEASON[1] & ref <= SEASON[2]]
fc <- rbindlist(lapply(files, function(f) {
  d <- if (grepl("parquet$", f)) as.data.table(read_parquet(f)) else
    fread(f, colClasses = c(location = "character", output_type_id = "character"))
  d <- d[target == "wk inc rsv hosp" & output_type == "quantile"]
  if (!nrow(d)) return(NULL)
  d[, .(hub = "rsv_nhsn", model = basename(dirname(f)),
        forecast_date = as.Date(reference_date) - 3,       # Wednesday due date
        reference_date = as.Date(reference_date), target, horizon = as.integer(horizon),
        target_end_date = as.Date(target_end_date), location = sprintf("%s", location),
        quantile = round(as.numeric(output_type_id), 3), value = as.numeric(value))]
}), fill = TRUE)
dir.create("data/hubs/rsv_nhsn", recursive = TRUE, showWarnings = FALSE)
write_parquet(fc, "data/hubs/rsv_nhsn/forecasts.parquet")
message(sprintf("RSV (NHSN): %d forecast rows, %d models, reference dates %s to %s",
                nrow(fc), uniqueN(fc$model), min(fc$reference_date), max(fc$reference_date)))
unlink(tmp, recursive = TRUE)
