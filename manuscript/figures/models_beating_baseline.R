# Figure S14: how the share of submitted models that beat a baseline depends on how good the baseline is.
# For each hub, the curve gives the % of submitted team models (those covering >= 50% of our forecasts)
# whose pairwise relative WIS against the hub baseline is lower than x, i.e. that would beat a baseline
# with WIS relative to the hub baseline of x. Same scale and same team models as Figure 3 (its 50% and 80%
# reference lines are where the curves cross 50% and 20%). The hub baseline is at x = 1. Markers: three of
# our flatline variants and, for each hub, the log-transformed flatline with the lowest relative WIS, at their
# relative WIS against the hub baseline (Figure 3), placed on the curves.
# Reads:  pipeline/output/tables/hub_pairwise_rel_wis.csv, variants_vs_hub_baseline.csv
# Writes: manuscript/output/Figure_S14.png and .pdf (number from items.csv)
source("manuscript/_setup.R")
source("manuscript/_wis_by_window.R")   # shared styling with Figure 3 (ONE_LINE)

FONT_SIZE <- 14
HUB_COL   <- c(covid = "#1baf7a", flu = "#4a3aa7", rsv_nhsn = "#e87ba4")
HUB_LAB   <- c(covid = "COVID-19", flu = "Influenza", rsv_nhsn = "RSV")
MARKERS   <- data.table(model = c("none_sym_wall", "sqrt_sym_w10", "sqrt_sym_wall", "best_log"),
                        shape = c(21, 22, 23, 25))
MARKER_LAB <- c(none_sym_wall = expression(paste("Flatline, raw, ", italic(w), " = ", infinity)),
                sqrt_sym_w10  = expression(paste("Flatline, square root, ", italic(w), " = 10")),
                sqrt_sym_wall = expression(paste("Flatline, square root, ", italic(w), " = ", infinity)),
                best_log      = expression(paste("Flatline, log, best ", italic(w))))
X_RANGE   <- c(0.6, 1.8)                   # relative WIS shown

team <- tab("hub_pairwise_rel_wis.csv")[type == "team" & target %in% names(HUB_COL)]
pct_beating <- function(t, x) 100 * mean(team[target == t, rel_wis] < x)
# step curves: the share changes only at each team model's relative WIS
curve <- team[, .(x = sort(c(X_RANGE, rel_wis))), by = target]
curve[, y := mapply(pct_beating, target, x)]
vh <- tab("variants_vs_hub_baseline.csv")[target %in% names(HUB_COL)]
best_log <- vh[grepl("^log_sym_", model)][order(rel_wis_vs_hub_baseline)][, .SD[1], by = target]
mk <- rbind(vh[model %in% MARKERS$model, .(target, model, x = rel_wis_vs_hub_baseline)],
            best_log[, .(target, model = "best_log", x = rel_wis_vs_hub_baseline)])
mk[, y := mapply(pct_beating, target, x)]
for (d in list(curve, mk)) d[, hub := factor(HUB_LAB[target], levels = HUB_LAB)]
mk[, model := factor(model, levels = MARKERS$model)]

p <- ggplot(curve, aes(x, y, colour = hub)) +
  geom_vline(xintercept = 1, colour = ONE_LINE$colour, alpha = ONE_LINE$alpha, linewidth = ONE_LINE$linewidth) +
  geom_step(direction = "vh", linewidth = 0.8) +   # the share jumps just after each model's value
  geom_point(data = mk, aes(shape = model), fill = "white", size = 3, stroke = 1.1) +
  scale_colour_manual(values = setNames(HUB_COL, HUB_LAB), name = NULL) +
  scale_shape_manual(values = setNames(MARKERS$shape, MARKERS$model), labels = MARKER_LAB, name = NULL) +
  scale_x_continuous(trans = "log10", breaks = c(0.6, 0.8, 1, 1.25, 1.5), limits = X_RANGE,
                     labels = function(x) format(x, drop0trailing = TRUE)) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(x = "WIS relative to the hub baseline", y = "Submitted models beating the baseline") +
  base_theme(FONT_SIZE) +
  theme(legend.position = "right", legend.text.align = 0) +
  guides(colour = guide_legend(order = 1, override.aes = list(linewidth = 1.2)),
         shape = guide_legend(order = 2))
p
save_fig(p, "models_beating_baseline", 9, 5)
