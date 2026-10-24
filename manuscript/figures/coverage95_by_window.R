# Figure S10: 95% prediction interval coverage against training window, styled as Figure 3
# (raw, square-root and log-transformed models). Grey line: nominal 95% coverage.
# Reads:  pipeline/output/tables/wis_by_window.csv
# Writes: manuscript/output/Figure_S10.png and .pdf (number from items.csv)
source("manuscript/_setup.R")
source("manuscript/_wis_by_window.R")

p <- plot_coverage("cov95", nominal = 95, ylab = "95% interval coverage")
p
save_fig(p, "coverage95_by_window", 10, 7)
