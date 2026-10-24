# Score real-time hub submissions on the same forecast cells as our baselines (see R/hubs.R).
# Usage: Rscript pipeline/scripts/03_score_hubs.R [targets...]
source("pipeline/R/setup.R")
source("pipeline/R/hubs.R")
args <- commandArgs(trailingOnly = TRUE)
for (tg in if (length(args)) args else names(hub_map)) score_hub(tg)
