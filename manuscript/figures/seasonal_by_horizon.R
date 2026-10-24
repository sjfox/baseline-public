# Figure S13: square-root flatline (w = 10 and all data) and log flatline (all data) vs the seasonal baseline
# on the most recent 3 years (log; the best seasonal variant, as in Figure 4) by forecast horizon, 1-10 weeks
# ahead (wILI, real-time data). (A) mean WIS; (B) relative WIS of each flatline: its mean WIS divided by the
# seasonal baseline's on the same forecasts (< 1 = flatline better), on a log scale as in Figure 4B.
# Forecasts whose target week falls outside October-April are not scored, so later horizons have fewer
# forecasts.
# Reads:  pipeline/output/tables/wili_long_horizon_summary.csv (from 07_wili_long_horizons.R)
# Writes: manuscript/output/Figure_S13.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

FONT_SIZE <- 13
MODELS <- data.table(model = c("sqrt_sym_w10", "sqrt_sym_wall", "log_sym_wall", "seasonal_last3y"),
                     id    = c("w10", "wall", "log_wall", "seasonal"),
                     col   = c(COL[["Flatline"]], COL[["Flatline"]], COL[["Flatline"]], COL[["Seasonal"]]),
                     lty   = c("solid", "42", "13", "solid"))
LABELS <- c(w10 = expression(paste("Flatline, square root, ", italic(w), " = 10")),
            wall = expression(paste("Flatline, square root, ", italic(w), " = ", infinity)),
            log_wall = expression(paste("Flatline, log, ", italic(w), " = ", infinity)),
            seasonal = "Seasonal baseline")

s <- tab("wili_long_horizon_summary.csv")[model %in% MODELS$model]
s[, Model := factor(MODELS$id[match(model, MODELS$model)], levels = MODELS$id)]
model_scales <- list(
  scale_colour_manual(values = setNames(MODELS$col, MODELS$id), labels = LABELS, name = NULL, drop = FALSE),
  scale_linetype_manual(values = setNames(MODELS$lty, MODELS$id), labels = LABELS, name = NULL, drop = FALSE),
  scale_x_continuous(breaks = 1:10))

pA <- ggplot(s, aes(horizon, mean_wis, colour = Model, linetype = Model)) +
  model_scales + geom_line(linewidth = 0.9) +
  labs(x = "Forecast horizon (weeks)", y = "WIS")
# relative WIS = flatline / seasonal mean WIS on identical forecasts = 1 - % improvement / 100
s[, rel := 1 - pct_improvement_vs_seasonal_last3y / 100]
pB <- ggplot(s[model != "seasonal_last3y"], aes(horizon, rel, colour = Model, linetype = Model)) +
  model_scales +
  geom_hline(yintercept = 1, colour = COL[["Seasonal"]], linewidth = 0.9) +   # the seasonal baseline (= 1)
  geom_line(linewidth = 0.9) +
  annotate("text", x = 10, y = 1.04, label = "seasonal better", hjust = 1, vjust = 0, size = FONT_SIZE / 3.6, colour = "grey35") +
  annotate("text", x = 10, y = 0.96, label = "flatline better", hjust = 1, vjust = 1, size = FONT_SIZE / 3.6, colour = "grey35") +
  scale_y_continuous(trans = "log10", breaks = c(0.25, 0.5, 0.75, 1, 1.5, 2),
                     labels = function(x) format(x, drop0trailing = TRUE)) +
  labs(x = "Forecast horizon (weeks)", y = "Relative WIS") +
  guides(colour = "none", linetype = "none")   # one legend, from panel A

p <- pA + pB + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A") &
  base_theme(FONT_SIZE) &
  theme(legend.position = "right", legend.key.width = unit(2.6, "lines"),
        legend.text.align = 0, plot.tag = element_text(face = "bold"))
p
save_fig(p, "seasonal_by_horizon", 11, 4.8)
