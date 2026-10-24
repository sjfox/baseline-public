# Figure 2: national 1-4-week-ahead forecasts, flatline vs drift.
# Raw data, 10 most recent differences, real-time data. Forecast fans every 4 weeks (every 4th
# weekly forecast date within each season), so consecutive fans meet end to end; wILI shows its
# nine evaluated seasons. Final data in black. Grey shading: periods evaluated in Figure 3 and Table 1.
# Layout: A COVID-19, B influenza, C wILI, D RSV; the left column (COVID-19, wILI: the longer series) is
# wider. Panels are labelled A-D with cowplot; the y-axis titles name the disease.
# Reads:  output/quantiles/<target>.parquet, output/tables/w10_by_reference_date.csv
# Writes: manuscript/output/Figure_2.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

FAN_EVERY  <- 4        # show the forecast from every 4th forecast date (= every 4 weeks)
COL_WIDTHS <- c(1.5, 1)   # relative widths of the left (COVID-19, wILI) and right (influenza, RSV) columns
SHADE      <- list(fill = "grey70", alpha = 0.15)   # evaluation periods
PANELS <- data.table(   # position in the 2 x 2 layout (A B / C D) = row order of this table
  target = c("covid", "flu", "ili_asof", "rsv_nhsn"),
  ylab   = c("COVID-19 admits", "Influenza admits", "wILI (%)", "RSV admits"))
LABEL_SIZE <- 14        # panel letters (cowplot labels, drawn in the top-left corner of each panel)

# Forecasts: national, raw flatline and drift, 10 differences; every 4th forecast date within each
# season (a new season starts after a gap of more than 5 weeks between forecast dates)
fans <- function(t) {
  q <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "quantiles", paste0(t, ".parquet"))))
  q <- q[location %in% NATIONAL & model %in% c("none_sym_w10", "none_nonsym_w10") &
           quantile %in% c(0.025, 0.5, 0.975)]
  if (t %in% SEASONAL_TARGETS) q <- q[month(reference_date) %in% SEASON_MONTHS]
  if (t == "covid") q <- q[between(snapshot, PERIODS$covid[1], PERIODS$covid[2])]
  if (t == "flu") q <- q[in_flu_seasons(reference_date)]
  od <- data.table(reference_date = sort(unique(q$reference_date)))
  od[, season := cumsum(c(1, diff(as.numeric(reference_date)) > 35))]
  keep <- od[, .SD[seq(1, .N, by = FAN_EVERY)], by = season]$reference_date
  q <- dcast(q[reference_date %in% keep], model + reference_date + horizon + target_end_date ~ quantile,
             value.var = "value")
  setnames(q, c("0.025", "0.5", "0.975"), c("lo", "med", "hi"))
  q[, family := factor(ifelse(grepl("_sym_", model), "Flatline", "Drift"), levels = c("Flatline", "Drift"))]
  q[]
}

# Final data over the plotted range (daily COVID-19 shown on the weekday of the forecast targets)
observed <- function(t, q) {
  tr <- load_target(t)$truth[location %in% NATIONAL & !is.na(value)]
  rng <- range(q$target_end_date)
  tr <- unique(tr[date >= rng[1] - 28 & date <= rng[2], .(date, value)])[order(date)]
  if (t == "covid") tr <- tr[as.integer(date - min(q$target_end_date)) %% 7 == 0]
  tr[, gap := cumsum(c(1, diff(as.numeric(date)) > 7 * 5))][]   # break the line at season gaps
}

# Evaluation periods: the forecast dates scored in Figure 3 and Table 1, from the first forecast
# date of each season to its last 4-week-ahead target, clipped to the plotted range
eval_periods <- function(t, lo, hi) {
  ev <- unique(tab("w10_by_reference_date.csv")[target == t, .(reference_date = as.Date(reference_date))])
  setorder(ev, reference_date)
  ev[, block := cumsum(c(1, diff(as.numeric(reference_date)) > 35))]
  ev <- ev[, .(xmin = max(min(reference_date), lo), xmax = min(max(reference_date) + 28, hi)), by = block]
  ev[xmin < xmax]
}

panel_plot <- function(i) {
  t <- PANELS$target[i]
  q <- fans(t); obs <- observed(t, q)
  ev <- eval_periods(t, min(obs$date, q$target_end_date), max(obs$date, q$target_end_date))
  ylabels <- if (t == "ili_asof") function(x) paste0(x, "%") else scales::comma
  ggplot(q, aes(target_end_date)) +
    # drawing order = layering: shading, then data, then forecasts on top
    geom_rect(data = ev, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE,
              fill = SHADE$fill, alpha = SHADE$alpha, colour = NA) +
    geom_line(data = obs, aes(date, value, group = gap), colour = "grey10", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = family, group = interaction(family, reference_date)),
                alpha = 0.3) +
    geom_line(aes(y = med, colour = family, group = interaction(family, reference_date)), linewidth = 0.6) +
    scale_family() + scale_family("fill") +
    scale_y_continuous(labels = ylabels) +
    scale_x_date(labels = scales::label_date_short(), expand = expansion(mult = 0.02)) +
    labs(x = NULL, y = PANELS$ylab[i])
}

panels <- lapply(seq_len(nrow(PANELS)), panel_plot)
legend <- cowplot::get_plot_component(panels[[1]] + theme(legend.position = "bottom"),
                                      "guide-box-bottom", return_all = TRUE)
grid <- cowplot::plot_grid(plotlist = lapply(panels, `+`, theme(legend.position = "none")),
                           ncol = 2, rel_widths = COL_WIDTHS, labels = "AUTO", label_size = LABEL_SIZE,
                           align = "hv", axis = "tblr",          # line up the axes of all four panels
                           hjust = 0, label_x = 0.005)
p <- cowplot::plot_grid(grid, legend, ncol = 1, rel_heights = c(1, 0.05))
p
save_fig(p, "national_forecasts", 9, 5)
