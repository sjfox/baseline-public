# Figure S1: data revisions and why they matter for RSV.
# (A) revision of the most recent observation: % change from the value reported at each forecast date
#     to its final value (positive = revised up), for the four real-time datasets (boxplot over
#     jittered location-dates; pseudo-log axis).
# (B) RSV (NHSN) and (C) RSV-NET: mean WIS split into dispersion, underprediction and overprediction,
#     drift vs flatline (raw data, w = 10), with real-time and with final data at identical forecast dates.
# Reads:  pipeline/output/tables/data_revisions_latest.csv, reporting_scores_by_version.csv
# Writes: manuscript/output/Figure_S1.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

FONT_SIZE <- 13
DATASETS <- c(covid = "COVID-19", flu = "Influenza", rsv_nhsn = "RSV (NHSN)", rsv_asof = "RSV-NET")
DIS_COL  <- c("COVID-19" = "#1baf7a", "Influenza" = "#4a3aa7", "RSV (NHSN)" = "#c98a0b", "RSV-NET" = "#e87ba4")
COMP_COL <- c(Dispersion = "#86b6ef", Underprediction = "#2a78d6", Overprediction = "#eb6834")
WIS_LAB  <- expression(paste("WIS (", italic(w), " = 10, raw)"))

# ---- A: revision of the most recent observation --------------------------------------------------
rv <- tab("data_revisions_latest.csv")[!is.na(pct_revision)]
rv[, dataset := factor(DATASETS[target], levels = DATASETS)]
set.seed(1)
pa <- ggplot(rv, aes(dataset, pct_revision)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_jitter(aes(colour = dataset), width = 0.25, height = 0, size = 0.5, alpha = 0.25, show.legend = FALSE) +
  geom_boxplot(width = 0.45, outlier.shape = NA, fill = NA, colour = "grey15", linewidth = 0.45) +
  scale_colour_manual(values = DIS_COL) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 1, base = 10),
                     breaks = c(-100, -10, 0, 10, 100, 1000),
                     labels = c("-100", "-10", "0", "10", "100", "1,000")) +
  labs(x = NULL, y = "Revision (%)", title = "A") +
  base_theme(FONT_SIZE) + theme(panel.grid.major.x = element_blank())

# ---- B, C: WIS decomposition, real-time vs final ------------------------------------------------
dc <- tab("reporting_scores_by_version.csv")[transformation == "none" & data %in% c("real-time", "final") &
                                              disease %in% c("rsv_nhsn", "rsv")]
dc <- melt(dc, id.vars = c("disease", "data", "family"), measure.vars = c("dispersion", "underpred", "overpred"),
           variable.name = "component")
dc[, component := factor(component, c("overpred", "underpred", "dispersion"), rev(names(COMP_COL)))]
dc[, Model := ifelse(family == "flatline", "Flatline", "Drift")]
# Bars at numeric positions: drift and flatline with real-time data, then with final data
dc[, x := (Model == "Flatline") + 2.5 * (data == "final")]
data_lab <- data.table(x = c(0.5, 3), label = c("Real-time", "Final"))
totals <- dc[, .(wis = sum(value)), by = .(disease, x)]

decomp <- function(d) {
  ggplot(dc[disease == d], aes(x, value, fill = component)) +
    geom_col(width = 0.8, colour = "white", linewidth = 0.4) +
    geom_text(data = totals[disease == d], aes(x, wis, label = sprintf("%.1f", wis)), inherit.aes = FALSE,
              vjust = -0.4, size = FONT_SIZE / 3.6, colour = "grey25") +
    geom_text(data = data_lab, aes(x, Inf, label = label), inherit.aes = FALSE,
              vjust = 1.4, size = FONT_SIZE / 3.2, fontface = "bold", colour = "grey30") +
    scale_x_continuous(breaks = c(0, 1, 2.5, 3.5), labels = rep(c("Drift", "Flatline"), 2)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    scale_fill_manual(values = COMP_COL, breaks = names(COMP_COL), name = NULL) +
    labs(x = NULL, y = WIS_LAB) +
    base_theme(FONT_SIZE) + theme(panel.grid.major.x = element_blank())
}
pb <- decomp("rsv_nhsn") + ggtitle("B. RSV (NHSN)")
pc <- decomp("rsv") + ggtitle("C. RSV-NET") + labs(y = NULL)

p <- pa / (pb + pc + plot_layout(guides = "collect")) +
  plot_layout(heights = c(1, 1)) & theme(legend.position = "bottom")
p
save_fig(p, "data_revisions", 8.5, 8.5)
