# Figure S6: Figure 3 with the log-transformed models added (WIS relative to each target's standard
# baseline against training window). Log-transformed curves are not drawn where they exceed 1.5x the
# largest WIS of the other models in a panel.
# Reads:  as Figure 3 (figures/wis_by_window.R)
# Writes: manuscript/output/Figure_S6.png and .pdf (number from items.csv)
source("manuscript/_setup.R")
source("manuscript/_wis_by_window.R")

p <- plot_fig3(exclude_log = FALSE) + labs(caption = NULL)
p
save_fig(p, "wis_by_window_with_log", 10, 7)
