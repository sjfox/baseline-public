# Figure 3: forecast performance against training window, one panel per target, as WIS relative to
# that target's standard baseline (1 = same as the standard baseline; < 1 = better).
# COVID-19, influenza and RSV: the hub's own posted baseline. Each of our variants forecasts every
# cell and is treated as a real-time submission: its mean WIS over the forecasts the hub baseline
# also made, divided by the hub baseline's mean WIS there.
# wILI (no hub): the seasonal baseline (main specification), on the same forecasts: mean WIS of each
# variant divided by the seasonal baseline's mean WIS.
# Flatline (blue) and drift (orange); raw and square-root data as line types (the log-transformed
# models are not shown); the "all data" window is drawn at the right as a point whose shape gives
# the transformation.
# Horizontal lines: the median (50%) and 80th percentile of submitted team models' pairwise
# relative WIS against the hub baseline, computed as the hubs do (pairwise tournament among team
# models covering >= 50% of forecasts, the hub baseline and the hub ensemble; see pairwise_rel_wis()
# in pipeline/R/analysis.R).
# Reads:  pipeline/output/tables/wis_by_window.csv, variants_vs_hub_baseline.csv, hub_reference.csv,
#         seasonal_comparison_scores.csv
# Writes: manuscript/output/Figure_3.png and .pdf (number from items.csv)
source("manuscript/_setup.R")
source("manuscript/_wis_by_window.R")

p <- plot_fig3(exclude_log = TRUE)
p
save_fig(p, "wis_by_window", 10, 7)
