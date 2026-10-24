# Figure S8: Figure 3 with every forecast scored on the log scale (WIS of log(x + 1) quantiles against
# log(x + 1) of the observation), for our variants, the hub baseline, the team models and the seasonal
# baseline alike. Raw, square-root and log-transformed models (as in Figure S6); log-transformed curves are
# not drawn where they exceed 1.5x the largest WIS of the other models in a panel (noted under the figure).
# Reads:  pipeline/output/tables/wis_by_window_log_scale.csv, variants_vs_hub_baseline_log_scale.csv,
#         hub_reference_log_scale.csv (from 08_log_scale_scoring.R); seasonal baseline log-scale WIS from
#         pipeline/output/scores/ili_asof_seasonal.parquet
# Writes: manuscript/output/Figure_S8.png and .pdf (number from items.csv)
source("manuscript/_setup.R")
source("manuscript/_wis_by_window.R")

p <- plot_fig3(exclude_log = FALSE, suffix = "_log_scale")
p
save_fig(p, "wis_by_window_log_scale", 10, 7.3)
