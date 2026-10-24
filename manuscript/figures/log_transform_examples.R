# Figure S7: why the log transform fails on spiky, small counts. Two real forecasts: (A) Puerto Rico,
# COVID-19, forecast made 5 Mar 2021 (daily, 1-28 days ahead); (B) United States, RSV-NET, forecast made
# 12 Oct 2024 (weekly, 1-4 weeks ahead). Square-root vs log flatline (w = 10): median and 95% interval,
# on a pseudo-log axis. Grey: the data as available at the forecast date (the 11 observations the model
# is trained on, i.e. the last 10 differences, in black); open circles after the forecast date: final
# data. The two medians are identical (the log median is drawn dashed over the square-root one).
# Reads:  the vintaged COVID-19 and RSV-NET data (load_target()). The forecasts are computed here with the
#         pipeline's forecast engine, because the saved quantiles cover only w = 10 / all data for the
#         scored horizons (and this figure needs every day for COVID-19).
# Writes: manuscript/output/Figure_S7.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

FONT_SIZE <- 13
HISTORY   <- c(covid = 70, rsv_asof = 30)   # time steps of data shown before the forecast date
ex <- list(
  list(t = "covid",    loc = "72", ref = as.Date("2021-03-05"), step = 1, H = 28),
  list(t = "rsv_asof", loc = "US", ref = as.Date("2024-10-12"), step = 7, H = 4))
TR_COL <- c(`Square root` = "#2a78d6", Log = "#eb6834")   # transform colours (this figure only)
ylog <- scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 1, base = 10),
  breaks = c(0, 10, 100, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8),
  labels = c("0", "10", "100", "1,000", "10,000", "100,000", "1 million", "10 million", "100 million"))

panel <- function(e) {
  d <- load_target(e$t)
  y <- d$tasks[location == e$loc & reference_date == e$ref, series][[1]]
  n <- length(y); k <- min(HISTORY[[e$t]], n)
  hist <- data.table(date = e$ref - e$step * ((k - 1):0), value = tail(y, k))
  hist[, training := seq_len(.N) > .N - 11]
  fc <- rbindlist(lapply(c("sqrt", "log"), function(tr) {
    set.seed(1)
    q <- forecast_quantiles_batch(y, tr, TRUE, 10, keep = seq_len(e$H), count_data = TRUE)[, , 1]
    data.table(Transform = ifelse(tr == "sqrt", "Square root", "Log"),
               date = e$ref + e$step * seq_len(e$H), lo = q[, QUANTILES == 0.025],
               med = q[, QUANTILES == 0.5], hi = q[, QUANTILES == 0.975])
  }))
  fc[, Transform := factor(Transform, levels = names(TR_COL))]
  obs <- d$truth[location == e$loc & date > e$ref & date <= e$ref + e$step * e$H]
  ggplot(fc, aes(date)) +
    geom_vline(xintercept = e$ref, colour = "grey60", linetype = "dashed", linewidth = 0.3) +
    geom_line(data = hist, aes(date, value), colour = "grey60", linewidth = 0.4) +
    geom_point(data = hist, aes(date, value, colour = training), size = 1.2, show.legend = FALSE) +
    scale_colour_manual(values = c(`FALSE` = "grey60", `TRUE` = "grey10")) +
    ggnewscale::new_scale_colour() +
    geom_ribbon(data = fc[Transform == "Log"], aes(ymin = lo, ymax = hi, fill = Transform), alpha = 0.18) +
    geom_ribbon(data = fc[Transform == "Square root"], aes(ymin = lo, ymax = hi, fill = Transform), alpha = 0.45) +
    geom_line(aes(y = hi, colour = Transform), linewidth = 0.5, linetype = "22") +
    geom_line(data = fc[Transform == "Square root"], aes(y = med, colour = Transform), linewidth = 0.9) +
    geom_line(data = fc[Transform == "Log"], aes(y = med, colour = Transform), linewidth = 0.9, linetype = "42") +
    geom_point(data = obs, aes(date, value), size = 2, shape = 21, fill = "white", colour = "grey10", stroke = 0.8) +
    ylog +
    scale_colour_manual(values = TR_COL, breaks = names(TR_COL), name = NULL) +
    scale_fill_manual(values = TR_COL, breaks = names(TR_COL), name = NULL) +
    scale_x_date(labels = scales::label_date_short()) +
    labs(x = NULL, y = "Count") +
    base_theme(FONT_SIZE)
}
p <- panel(ex[[1]]) + panel(ex[[2]]) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "right", plot.tag = element_text(face = "bold", size = FONT_SIZE + 2))
p
save_fig(p, "log_transform_examples", 12, 4)
