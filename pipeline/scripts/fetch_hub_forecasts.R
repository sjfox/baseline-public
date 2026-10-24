# Download real-time forecast submissions from the COVID-19 Forecast Hub and FluSight at pinned commits and
# write them in one schema to data/hubs/<hub>/forecasts.parquet:
#   hub, model, forecast_date, reference_date, target, horizon, target_end_date, location,
#   quantile, value
# Usage (from the project root; needs curl and python3):
#   Rscript pipeline/scripts/fetch_hub_forecasts.R [covid] [flu]
# Set HUB_OUT_DIR to write somewhere other than data/hubs, and KEEP_RAW=1 to keep the extracted
# repository files (data/hubs/raw/). The COVID-19 archive is several GB and is filtered while
# streaming to incident-hospitalization rows.
source("pipeline/R/setup.R")

HUBS <- list(
  covid = list(repo = "reichlab/covid19-forecast-hub", sha = "3a6997b6abdcfabe321dd059ebe32334095d49b5",
               subdir = "data-processed", from = "2020-11-10", to = "2023-10-09", grep = "inc hosp"),
  flu   = list(repo = "cdcepi/FluSight-forecast-hub", sha = "ee477e749adbb253d908af2e2872b70eeb17bfa4",
               subdir = "model-output", from = "2023-10-14", to = "2025-05-31"))
out_root <- Sys.getenv("HUB_OUT_DIR", file.path(DATA_DIR, "hubs"))
raw_root <- file.path(DATA_DIR, "hubs", "raw")
extractor <- file.path("pipeline", "scripts", "hub_data", "extract_from_tarball.py")

download_hub <- function(h, dest) {
  url <- sprintf("https://codeload.github.com/%s/tar.gz/%s", h$repo, h$sha)
  args <- c("--subdir", h$subdir, "--dest", shQuote(dest),
            if (!is.null(h$from)) c("--date-from", h$from, "--date-to", h$to),
            if (!is.null(h$grep)) c("--grep", shQuote(h$grep)))
  cmd <- sprintf("curl -sSfL %s | python3 %s %s", url, extractor, paste(args, collapse = " "))
  message(cmd)
  if (system(cmd) != 0) stop("download failed: ", h$repo)
}

pad_location <- function(x) {
  x <- trimws(as.character(x))
  ifelse(grepl("^[0-9]$", x), paste0("0", x), x)
}
model_files <- function(dir, pattern) list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE)

# COVID-19 Forecast Hub: data-processed/<model>/<date>-<model>.csv, targets "<k> day ahead inc hosp"
process_covid <- function(dir) {
  rbindlist(lapply(model_files(dir, "\\.csv$"), function(f) {
    x <- fread(f, colClasses = "character", showProgress = FALSE)
    if (!nrow(x)) return(NULL)
    setnames(x, tolower(names(x)))
    k <- suppressWarnings(as.integer(sub("^\\s*(\\d+) day ahead inc hosp\\s*$", "\\1", x$target)))
    x <- x[!is.na(k) & k >= 1 & k <= 28 & type == "quantile" & !is.na(quantile) & quantile != "NA"]
    if (!nrow(x)) return(NULL)
    x[, .(hub = "covid", model = basename(dirname(f)), forecast_date = as.Date(forecast_date),
          reference_date = as.Date(forecast_date), target,
          horizon = as.integer(sub("^\\s*(\\d+) day.*$", "\\1", target)),
          target_end_date = as.Date(target_end_date), location = pad_location(location),
          quantile = round(as.numeric(quantile), 3),
          value = suppressWarnings(as.numeric(value)))]          # "NA" strings -> NA, dropped below
  }))[location == "US" | grepl("^[0-9]{2}$", location)][!is.na(value)]
}

# FluSight: model-output/<model>/<date>-<model>.csv, target "wk inc flu hosp"
process_flu <- function(dir) {
  rbindlist(lapply(model_files(dir, "\\.(csv|parquet)$"), function(f) {
    x <- if (grepl("parquet$", f)) as.data.table(arrow::read_parquet(f)) else
      fread(f, colClasses = list(character = c("location", "output_type_id")), showProgress = FALSE)
    x <- x[target == "wk inc flu hosp" & output_type == "quantile"]
    if (!nrow(x)) return(NULL)
    x[, .(hub = "flu", model = basename(dirname(f)), forecast_date = as.Date(reference_date),
          reference_date = as.Date(reference_date), target, horizon = as.integer(horizon),
          target_end_date = as.Date(target_end_date), location = pad_location(location),
          quantile = round(as.numeric(output_type_id), 3), value = as.numeric(value))]
  }))
}

args <- commandArgs(trailingOnly = TRUE)
for (hub in if (length(args)) args else names(HUBS)) {
  raw <- file.path(raw_root, hub)
  if (!dir.exists(raw)) download_hub(HUBS[[hub]], raw)
  dir.create(file.path(out_root, hub), recursive = TRUE, showWarnings = FALSE)
  res <- switch(hub, covid = process_covid(raw), flu = process_flu(raw))
  res <- unique(res)[order(model, forecast_date, location, horizon, quantile)]
  arrow::write_parquet(res, file.path(out_root, hub, "forecasts.parquet"))
  message(sprintf("[%s] %d rows, %d models, %d forecast dates", hub, nrow(res), uniqueN(res$model),
                  uniqueN(res$forecast_date)))
  if (Sys.getenv("KEEP_RAW") != "1") unlink(raw, recursive = TRUE)
}
