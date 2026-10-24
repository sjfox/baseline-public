# Run + score the full trend-baseline grid. Usage: Rscript pipeline/scripts/02_run_trend.R [targets...]
# Checkpoints per origin date in output/runs/<target>/; rerunning resumes where it stopped.
source("pipeline/R/setup.R")
args <- commandArgs(trailingOnly = TRUE)
targets <- if (length(args)) args else c("flu", "rsv", "rsv_asof", "rsv_nhsn", "rsv_nhsn_final", "ili_asof", "covid")
for (tg in targets) {
  t0 <- Sys.time()
  run_target(tg)
  message(sprintf("[%s] finished in %.1f min", tg, as.numeric(Sys.time() - t0, units = "mins")))
}
