# Source every pipeline module. Run scripts from the project root.
suppressPackageStartupMessages({ library(data.table); library(arrow) })
for (f in c("config.R", "baseline_model.R", "scoring.R", "data.R", "runner.R"))
  source(file.path("pipeline", "R", f))
for (d in c("vintaged", "runs", "logs", "scores", "quantiles", "tables"))
  dir.create(file.path(OUT_DIR, d), recursive = TRUE, showWarnings = FALSE)
