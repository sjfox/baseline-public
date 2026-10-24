# Figure S11: WIS decomposition for the model variants in Table 1. For each main target and each window
# (w = 10 and all data), mean WIS of the square-root flatline (Table 1's row model) and the alternatives
# it is compared with in Table 1 (square-root drift, raw flatline, log flatline), split into dispersion,
# underprediction and overprediction. Same forecasts as Figure 3 / Table 1.
# Bars taller than the panel (COVID-19 log flatline, w = 10) are cut at the top, with their total WIS printed.
# The standard baseline (hub baseline / seasonal) is not shown: it is scored on a different set of forecasts.
# Reads:  pipeline/output/tables/wis_by_window.csv
# Writes: manuscript/output/<number>.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

FONT_SIZE <- 15
CAP       <- 1.6                 # panel height: CAP x the tallest bar other than the log flatline
COMP_COL  <- c(Dispersion = "#86b6ef", Underprediction = "#2a78d6", Overprediction = "#eb6834")   # as Figure S1
VARIANTS  <- c(sqrt_sym = "Flatline,\nsquare root", sqrt_nonsym = "Drift,\nsquare root",
               none_sym = "Flatline,\nraw", log_sym = "Flatline,\nlog")
WINDOWS   <- c("10" = "italic(w) == 10", all = "italic(w) == infinity")   # plotmath, for label_parsed

set <- intersect(FIG_ORDER, MAIN)
lv  <- panel_levels(set)
w <- tab("wis_by_window.csv")[target %in% set & window %in% names(WINDOWS)]
w[, variant := sub("_w.*$", "", model)]
w <- w[variant %in% names(VARIANTS)]
w[, `:=`(panel = factor(lv[target], levels = lv),
         variant = factor(VARIANTS[variant], levels = VARIANTS),
         win = factor(WINDOWS[as.character(window)], levels = WINDOWS))]
caps <- w[variant != VARIANTS[["log_sym"]], .(cap = CAP * max(wis)), by = panel]
caps <- caps[panel %in% w[caps, on = "panel"][wis > cap, panel]]   # only panels with a bar to cut
d <- melt(w, id.vars = c("panel", "variant", "win", "wis"), measure.vars = c("dispersion", "underpred", "overpred"),
          variable.name = "component")
d[, component := factor(component, c("overpred", "underpred", "dispersion"), rev(names(COMP_COL)))]
cut <- caps[w, on = "panel"][wis > cap]

p <- ggplot(d, aes(variant, value, fill = component)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
  geom_blank(data = caps, aes(x = 1, y = cap), inherit.aes = FALSE) +
  geom_text(data = cut, aes(variant, cap, label = sprintf("%.0f", wis)), inherit.aes = FALSE,
            vjust = 1.3, size = FONT_SIZE / 3.6, fontface = "bold", colour = "white") +
  facet_grid(panel ~ win, scales = "free_y", labeller = labeller(win = label_parsed)) +
  scale_fill_manual(values = COMP_COL, breaks = names(COMP_COL), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = NULL, y = "WIS") +
  base_theme(FONT_SIZE) +
  theme(panel.grid.major.x = element_blank(), legend.position = "bottom",
        strip.text.y = element_text(angle = -90, hjust = 0.5), strip.text.x = element_text(hjust = 0.5, size = FONT_SIZE + 2),
        panel.spacing.y = unit(1.2, "lines"))
# cut bars at each panel's cap: facet_grid has no per-panel ylim, so clip the data instead
d[caps, on = "panel", cap := i.cap]
d[, cum := cumsum(value), by = .(panel, variant, win)]
d[!is.na(cap), value := pmax(0, pmin(value, cap - (cum - value)))]
p$data <- d
p
save_fig(p, "wis_decomposition", 10.5, 10.5)
