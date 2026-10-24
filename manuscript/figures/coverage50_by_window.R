# Figure S9: 50% prediction interval coverage against training window, styled as Figure 3
# (raw, square-root and log-transformed models). Grey line: nominal 50% coverage.
# Reads:  pipeline/output/tables/wis_by_window.csv
# Writes: manuscript/output/Figure_S9.png and .pdf (number from items.csv)
source("manuscript/_setup.R")
source("manuscript/_wis_by_window.R")

p <- plot_coverage("cov50", nominal = 50, ylab = "50% interval coverage")
p
save_fig(p, "coverage50_by_window", 10, 7)
