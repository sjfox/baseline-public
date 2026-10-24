# Figure S2: mean WIS by forecast horizon, flatline vs drift (raw data, w = 10), for the four main
# targets and RSV-NET (real-time data).
# Reads:  pipeline/output/tables/w10_by_horizon.csv
# Writes: manuscript/output/Figure_S2.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

FONT_SIZE <- 13
SET <- c(intersect(FIG_ORDER, MAIN), "rsv_asof")
LABELS <- c(PANEL_LABEL[intersect(FIG_ORDER, MAIN)], rsv_asof = "RSV-NET")
lv <- setNames(paste0(LETTERS[seq_along(SET)], ". ", LABELS[SET]), SET)

h <- tab("w10_by_horizon.csv")[target %in% SET & model %in% c("none_sym_w10", "none_nonsym_w10")]
h[, `:=`(family = factor(ifelse(grepl("_sym_", model), "Flatline", "Drift"), levels = c("Flatline", "Drift")),
         panel = factor(lv[target], levels = lv))]
p <- ggplot(h, aes(horizon, wis, colour = family)) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  facet_wrap(~panel, scales = "free_y", ncol = 3) +
  scale_family() +
  labs(x = "Forecast horizon (weeks)", y = expression(paste("WIS (", italic(w), " = 10, raw)"))) +
  base_theme(FONT_SIZE) + theme(legend.position = c(0.83, 0.25), legend.justification = "center")
p
save_fig(p, "wis_by_horizon", 9, 5.5)
