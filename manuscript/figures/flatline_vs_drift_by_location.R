# Figure S3: flatline / drift WIS ratio by location (raw data, w = 10); < 1 = flatline better.
# One column per dataset (COVID-19, influenza, RSV, then wILI above RSV-NET), each with its own x-axis.
# Points coloured by which model is better.
# Reads:  pipeline/output/tables/w10_by_location.csv
# Writes: manuscript/output/Figure_S3.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

FONT_SIZE <- 12
Y_TEXT    <- 9.5          # location labels
SETS <- list(covid = "COVID-19", flu = "Influenza", rsv_nhsn = "RSV", ili_asof = "wILI", rsv_asof = "RSV-NET")

l <- dcast(tab("w10_by_location.csv")[target %in% names(SETS) & model %in% c("none_sym_w10", "none_nonsym_w10")],
           target + location ~ model, value.var = "wis")
l[, `:=`(ratio = none_sym_w10 / none_nonsym_w10, label = loc_label(location))]
l[, better := ifelse(ratio < 1, "Flatline", "Drift")]

panel <- function(t, letter) {
  d <- l[target == t][order(ratio)]
  d[, label := factor(label, levels = label)]
  ggplot(d, aes(ratio, label, colour = better)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_point(size = 1.8) +
    scale_colour_manual(values = COL[c("Flatline", "Drift")], guide = "none") +
    labs(title = paste0(letter, ". ", SETS[[t]]), x = NULL, y = NULL) +
    base_theme(FONT_SIZE) + theme(axis.text.y = element_text(size = Y_TEXT))
}
p <- (panel("covid", "A") | panel("flu", "B") | panel("rsv_nhsn", "C") |
        (panel("ili_asof", "D") / panel("rsv_asof", "E"))) +
  plot_annotation(caption = expression(paste("WIS flatline / WIS drift (", italic(w), " = 10, raw)")),
                  theme = theme(plot.caption = element_text(hjust = 0.5, size = FONT_SIZE + 1)))
p
save_fig(p, "flatline_vs_drift_by_location", 11, 10)
