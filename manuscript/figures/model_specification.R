# Figure 1: how the training window and transformation shape the forecasts.
# Example forecasts for COVID-19, United States, made from the archived snapshot of 2022-01-10
# (data through 2022-01-07): flatline vs drift, 1-28 days ahead, for raw / square-root data and
# the 10 most recent differences (w = 10) / all data (w = ∞).
# Reads:  the vintaged COVID-19 data (load_target("covid")). The forecasts are computed here, with the
#         pipeline's forecast engine and the same seed as the main run, because the saved quantile forecasts
#         cover only days 7, 14, 21 and 28 and this figure shows every day from 1 to 28.
# Writes: manuscript/output/Figure_1.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

cv <- load_target("covid")
task <- cv$tasks[location == "US" & snapshot == as.Date("2022-01-10")]
y <- task$series[[1]]; origin <- task$reference_date
dates <- origin - rev(seq_along(y) - 1L)
days <- 1:28
configs <- CJ(transformation = c("none", "sqrt"), window = c(10, Inf), symmetrize = c(TRUE, FALSE), sorted = FALSE)
set.seed(task_seed("covid", "US", origin))
fc <- rbindlist(lapply(seq_len(nrow(configs)), function(i) {
  g <- configs[i]
  q <- forecast_quantiles(y, g$transformation, g$symmetrize, g$window, keep = days)
  data.table(g, date = origin + days, lo = q[, QUANTILES == 0.025], med = q[, QUANTILES == 0.5],
             hi = q[, QUANTILES == 0.975])
}))
# Panel titles as plotmath (parsed by the facet labeller): "A. w = 10, raw" ... "D. w = ∞, square root"
PANELS <- c('bold("A. "*bolditalic(w)*" = 10, raw")',
            'bold("B. "*bolditalic(w)*" = 10, square root")',
            'bold("C. "*bolditalic(w)*" = ")*infinity*bold(", raw")',
            'bold("D. "*bolditalic(w)*" = ")*infinity*bold(", square root")')
label_panel <- function(d) {
  d[, panel := factor(PANELS[1 + (transformation == "sqrt") + 2 * is.infinite(window)], levels = PANELS)]
  d[]
}
fc[, family := ifelse(symmetrize, "Flatline", "Drift")]
fc <- label_panel(fc)
obs <- label_panel(CJ(transformation = c("none", "sqrt"), window = c(10, Inf), i = seq_along(y))[
  , `:=`(date = dates[i], value = y[i])][, training := is.infinite(window) | i > length(y) - 11])
later <- cv$truth[location == "US" & date > origin & date <= origin + 28]
p <- ggplot() +
  geom_line(data = obs[training == FALSE], aes(date, value), colour = "grey70", linewidth = 0.3) +
  geom_line(data = obs[training == TRUE], aes(date, value, group = panel), colour = "grey10", linewidth = 0.35) +
  geom_point(data = later, aes(date, value), colour = "grey45", size = 0.35) +
  geom_ribbon(data = fc, aes(date, ymin = lo, ymax = hi, fill = family), alpha = 0.25) +
  geom_line(data = fc, aes(date, med, colour = family), linewidth = 0.6) +
  facet_wrap(~panel, ncol = 2, labeller = label_parsed) +
  scale_family() + scale_family("fill") +
  coord_cartesian(ylim = c(0, max(fc$hi, y, later$value, na.rm = TRUE) * 1.03)) +
  scale_x_date(breaks = as.Date(c("2021-01-01", "2021-07-01", "2022-01-01")), date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = "COVID-19 Hospital Admits, U.S.") +
  theme(panel.spacing.x = unit(1.5, "lines"),
        legend.position = "right",
        strip.text = element_text(size = 11.5, face = "bold", hjust = 0))   # panel titles
p
save_fig(p, "model_specification", 8, 4)
