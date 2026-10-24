# Figure S5: why COVID-19 drift models show a 7-day sawtooth in WIS against training window.
# The drift slope is (y[n] - y[n-w]) / w, so it depends only on the first and last values in the
# window. One series with a Friday origin and windows of 5-13 days (each weekday, plus the previous
# weekend): windows starting on a weekend dip give steeper slopes.
# Reads:  the vintaged COVID-19 data (load_target("covid"))
# Writes: manuscript/output/Figure_S5.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

EX_LOC <- "12"; EX_REF <- as.Date("2022-07-08")   # Florida, Friday origin, steady period
EX_W <- 5:13
H <- 28
WEEKEND <- c("Saturday", "Sunday")

d  <- load_target("covid")
y  <- d$tasks[location == EX_LOC & reference_date == EX_REF, series][[1]]
n  <- length(y)
obs <- data.table(date = EX_REF - (n - 1):0, value = y)[date > EX_REF - 22]
obs[, weekend := weekdays(date) %in% WEEKEND]
fut <- d$truth[location == EX_LOC & between(date, EX_REF + 1, EX_REF + H)]

# slope lines from y[n-w] through the origin, extended to the 4-week horizon
sl <- rbindlist(lapply(EX_W, function(w) {
  b <- (y[n] - y[n - w]) / w
  data.table(w = w, start = weekdays(EX_REF - w), date = EX_REF + c(-w, H), value = y[n] + b * c(-w, H))
}))
sl[, weekend_start := start %in% WEEKEND]
lab <- sl[date == EX_REF + H][, label := paste0(w, " days (", substr(start, 1, 3), ")")]

p <- ggplot() +
  geom_vline(xintercept = EX_REF, colour = "grey55", linewidth = 0.3) +
  geom_line(data = sl, aes(date, value, group = w, colour = weekend_start), linewidth = 0.5) +
  geom_line(data = obs, aes(date, value), colour = "grey70", linewidth = 0.3) +
  geom_point(data = fut, aes(date, value), colour = "grey75", size = 1.1) +
  geom_point(data = obs, aes(date, value, fill = weekend), shape = 21, size = 2, colour = "grey20") +
  ggrepel::geom_text_repel(data = lab, aes(date, value, label = label, colour = weekend_start),
                           hjust = 0, nudge_x = 1, direction = "y", segment.colour = NA,
                           size = 3, box.padding = 0.1, min.segment.length = Inf, show.legend = FALSE) +
  annotate("text", x = EX_REF, y = Inf, label = paste0("Forecast origin (", weekdays(EX_REF), ")"), hjust = 1.03, vjust = 1.3,
           size = 3, colour = "grey35") +
  scale_fill_manual(values = c(`TRUE` = "grey20", `FALSE` = "white"),
                    labels = c(`TRUE` = "Saturday or Sunday", `FALSE` = "Weekday"), name = "Day") +
  scale_colour_manual(values = c(`TRUE` = COL[["Drift"]], `FALSE` = "grey50"),
                      labels = c(`TRUE` = "Starts on a weekend", `FALSE` = "Starts on a weekday"),
                      name = "Drift slope") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d", expand = expansion(mult = c(0.02, 0.14))) +
  labs(x = NULL, y = "Daily hospital admissions, Florida") +
  theme(legend.position = "right", legend.justification = "center")
p
save_fig(p, "covid_weekday_mechanism", 9.5, 4.5)
