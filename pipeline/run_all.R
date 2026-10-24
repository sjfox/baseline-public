# Run the full analysis from the downloaded data (see README.md). Run from the project root:
#   Rscript pipeline/run_all.R
# Forecast runs checkpoint per forecast origin in pipeline/output/runs/ and resume where they stopped.
steps <- c("pipeline/scripts/01_build_data.R",
           "pipeline/scripts/02_run_trend.R",
           "pipeline/scripts/03_score_hubs.R",
           "pipeline/scripts/04_run_seasonal.R",
           "pipeline/scripts/05_analysis.R",
           "pipeline/scripts/06_rsv_data_versions.R",
           "pipeline/scripts/07_wili_long_horizons.R",
           "pipeline/scripts/08_log_scale_scoring.R")
for (s in steps) {
  message("\n=== ", s, " ===")
  status <- system2("Rscript", s)
  if (status != 0) stop("Step failed: ", s)
}
