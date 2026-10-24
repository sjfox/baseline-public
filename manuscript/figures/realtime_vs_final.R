# Figure S4: real-time vs final data on identical forecast dates and locations, RSV-NET and RSV (NHSN):
# mean WIS against training window (raw and square-root models), styled as Figure 3. Left column:
# models fit to the data as available at each forecast date; right: the same forecasts from final data.
# Both scored against final data.
# Reads:  pipeline/output/tables/realtime_vs_final.csv
# Writes: manuscript/output/Figure_S4.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

FONT_SIZE   <- 13
SHAPES      <- c(Raw = 21, `Square root` = 22)
POINT_ALPHA <- 0.6
ROWS <- c(rsv_asof = "RSV-NET", rsv_nhsn = "RSV (NHSN)")

a <- tab("realtime_vs_final.csv")[!grepl("^log", model)]
a <- label_rows(window_x(a), unique(a$target))
a[, `:=`(row = factor(ROWS[target], levels = ROWS),
         data = factor(data, levels = c("real-time", "final"), labels = c("Real-time data", "Final data")))]
p <- ggplot(a[window != "all"], aes(x, wis, colour = family)) +
  geom_line(aes(linetype = transform), linewidth = 0.6) +
  geom_point(data = a[window == "all"], aes(shape = transform, fill = family), size = 3,
             alpha = POINT_ALPHA, stroke = 0.9) +
  facet_grid(row ~ data, scales = "free_y") +
  scale_family() + scale_family("fill") + scale_transform() +
  scale_shape_manual(values = SHAPES, name = NULL) +
  scale_x_continuous(trans = "log10", breaks = c(5, 10, 20, 50)) +
  scale_y_continuous(trans = "log10", breaks = scales::breaks_pretty(n = 6)) +
  labs(x = XLAB_WINDOW, y = "WIS") +
  base_theme(FONT_SIZE) +
  theme(strip.text = element_text(size = FONT_SIZE + 1, face = "bold", hjust = 0.5),
        strip.text.y = element_text(angle = -90, hjust = 0.5),
        legend.position = "right", legend.key.width = unit(2.4, "lines")) +
  guides(colour = guide_legend(order = 1, override.aes = list(linewidth = 1.2, shape = NA)),
         fill = "none",
         linetype = guide_legend(order = 2, override.aes = list(linewidth = 1.1)),
         shape = guide_legend(order = 2, override.aes = list(fill = "grey60", alpha = POINT_ALPHA)))
p
save_fig(p, "realtime_vs_final", 10, 7)
