# Paths, model grid, targets, evaluation periods and hub settings.

# All scripts are run from the project root (the folder containing data/ and pipeline/).
PROJECT_ROOT <- normalizePath(Sys.getenv("BASELINE_ROOT", "."))
if (!dir.exists(file.path(PROJECT_ROOT, "data")) ||
    !dir.exists(file.path(PROJECT_ROOT, "pipeline"))) {
  stop("Run from the project root (folder containing data/ and pipeline/), ",
       "or set BASELINE_ROOT.")
}
DATA_DIR <- file.path(PROJECT_ROOT, "data")
OUT_DIR  <- file.path(PROJECT_ROOT, "pipeline", "output")

# ---- Trend baseline model grid -------------------------------------------------------
TRANSFORMATIONS  <- c("none", "sqrt", "log")   # y, sqrt(y + 1), log(y + 1)
TRANSFORM_OFFSET <- 1
SYMMETRIZE       <- c(TRUE, FALSE)             # TRUE = flatline, FALSE = drift
NSIM             <- 10000                      # simulated trajectories per forecast
QUANTILES        <- round(c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99), 3)

# Training window = number of most recent first differences (Inf = all available history).
WINDOWS_WEEKLY <- c(3:53, Inf)
# Daily data: dense grid for short windows, sparser for long ones, plus every whole number of
# weeks up to 390 days (so day-of-week composition can be held fixed and the grid passes one year).
WINDOWS_DAILY  <- c(sort(unique(c(3:60, seq(65, 90, 5), seq(100, 150, 10), seq(180, 390, 30),
                                   seq(7, 390, 7)))), Inf)

# Scored horizons, in time steps of the series; both are reported as 1-4 weeks ahead.
HORIZONS_WEEKLY <- 1:4
HORIZONS_DAILY  <- c(7, 14, 21, 28)

# Windows whose full quantile forecasts are saved (used for plotting forecasts).
SAVE_QUANTILE_WINDOWS <- c(10, Inf)

BASE_SEED <- 20240101L
N_CORES   <- max(1L, parallel::detectCores() - 1L)

# ---- Targets and evaluation periods -----------------------------------------------------
# covid, flu, rsv_asof and ili_asof use every archived snapshot available. The final-data targets
# (rsv, rsv_nhsn_final) use the forecast dates and locations of their real-time counterparts.
PERIODS <- list(
  # COVID-19: COVID-19 Forecast Hub forecast weeks (Monday snapshots), 2021-02-01 to 2023-10-09.
  # Every hub week in this range has an archived snapshot; before it, 5 of 8 hub weeks are missing.
  # Filtered on the snapshot (hub forecast date), so each location is forecast once per hub week.
  covid = c(as.Date("2021-02-01"), as.Date("2023-10-09")),
  # RSV (NHSN): CDC RSV Forecast Hub 2025-26 forecast weeks, reference dates 2025-11-22 to
  # 2026-05-30 (origins = reference date - 7). No NHSN releases or hub rounds Sep 24 - Nov 19, 2025.
  rsv_nhsn = c(as.Date("2025-11-15"), as.Date("2026-05-23"))
)
# Influenza: FluSight forecast weeks (hub reference date = our forecast origin + 7 days), 2023-24
# and 2024-25 seasons. Origins outside these windows have no hub forecasts to compare against.
FLU_SEASONS <- list(as.Date(c("2023-10-07", "2024-04-27")), as.Date(c("2024-11-16", "2025-05-24")))
in_flu_seasons <- function(d) Reduce(`|`, lapply(FLU_SEASONS, function(w) d >= w[1] & d <= w[2]))

# RSV and wILI are evaluated on respiratory-season forecast dates (October-April).
SEASON_MONTHS <- c(10, 11, 12, 1, 2, 3, 4)
# First forecast date evaluated for wILI (2014-15 season), in the main analysis and the seasonal-
# baseline comparison alike, so both cover whole seasons and the same forecasts.
SEASONAL_START <- as.Date("2014-10-01")
ILI_MAIN <- "ili_asof"
# wILI history starts with the first season of year-round reporting (2002-03). Before that only
# October-May was reported, so the history would contain only in-season week-to-week changes.
ILI_HISTORY_START <- as.Date("2002-10-01")

# COVID-19 snapshots: the HealthData.gov release the COVID-19 Forecast Hub used for each forecast
# week (chosen by scripts/covid_hub_vintage.R; see data_manifest/covid_hub_vintage.csv), filed under
# the hub week's Monday.
COVID_SNAPSHOT_DIR <- "archive_hub_vintage"

MAIN_TARGETS <- c("covid", "flu", "rsv_nhsn", "ili_asof")   # real-time (archived / versioned) data
SUPP_TARGETS <- c("rsv_asof", "rsv", "rsv_nhsn_final")      # RSV-NET (real-time, final), RSV NHSN (final)
ALL_TARGETS  <- c(MAIN_TARGETS, SUPP_TARGETS)

# National series are excluded from pooled scores for the count targets, as forecast hubs do:
# national counts are on a much larger scale and would dominate mean WIS (and RSV-NET's national
# series is the network-wide rate x US population, not the sum of the states). They are reported
# separately. wILI keeps its national region: all regions are on the same percentage scale.
NATIONAL_EXCLUDED_TARGETS <- c("covid", "flu", "rsv_nhsn", "rsv_asof", "rsv", "rsv_nhsn_final")

# Models referred to throughout the analysis
FOCAL_MODEL    <- "sqrt_sym_w10"    # square-root flatline, 10 most recent differences
STANDARD_MODEL <- "none_sym_wall"   # flatline on all raw data, as used by current hubs

# ---- Forecast hubs ----------------------------------------------------------------------
# Hub comparisons: main targets with hub forecasts (RSV-NET and wILI are not compared with hub models)
HUB_BASELINE <- c(covid = "COVIDhub-baseline", flu = "FluSight-baseline", rsv_nhsn = "RSVHub-baseline")
HUB_ENSEMBLE <- list(covid = c("COVIDhub-4_week_ensemble", "COVIDhub-ensemble"),   # in order of preference
                     flu = "FluSight-ensemble", rsv_nhsn = "RSVHub-ensemble")
HUB_OWN_MODELS <- "^(COVIDhub|COVIDhub_CDC|FluSight|RSVHub|hub)[-_]"   # hub-run models, not team submissions

model_id <- function(transformation, symmetrize, window) {
  paste0(transformation, "_", ifelse(symmetrize, "sym", "nonsym"), "_w",
         ifelse(is.infinite(window), "all", window))
}
