# Figure S12: seasonal baseline variants (wILI, real-time data): mean WIS against the number of most
# recent years of history used to fit the seasonal model ("All" = all history from 2002-03), with and
# without the log transform. The main specification is log, all data. All variants are scored on the
# same forecasts as the flatline comparison (Figure 4).
# Reads:  pipeline/output/tables/seasonal_variants.csv
# Writes: manuscript/output/Figure_S12.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

FONT_SIZE <- 13
ALL_X     <- 12                                    # x position of the "all history" points
TR_COL    <- c(Log = COL[["Seasonal"]], Raw = "grey35")

v <- tab("seasonal_variants.csv")
v[, `:=`(transform = factor(ifelse(grepl("nolog", seasonal_variant), "Raw", "Log"), levels = names(TR_COL)),
         years = suppressWarnings(as.integer(sub("^.*last([0-9]+)y$", "\\1", seasonal_variant))))]
v[is.na(years), years := ALL_X]
p <- ggplot(v, aes(years, seasonal_wis, colour = transform)) +
  geom_line(data = v[years <= 10], linewidth = 0.7) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = TR_COL, name = NULL) +
  scale_x_continuous(breaks = c(1:10, ALL_X), labels = c(1:10, "All")) +
  labs(x = "Years of history used", y = "WIS") +
  base_theme(FONT_SIZE) + theme(legend.position = "right")
p
save_fig(p, "seasonal_variants", 7, 4.5)
