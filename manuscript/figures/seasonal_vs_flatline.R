# Figure 4: the best flatline vs the best seasonal baseline for wILI (real-time data): the log-transformed
# flatline trained on all differences (log_sym_wall, lowest WIS of the trend grid for wILI) vs the
# log-transformed seasonal model trained on the most recent 3 years (seasonal_last3y, lowest WIS of the
# seasonal variants; Figure S12). Both scored on the same forecasts.
# (A) national 1-4-week-ahead forecasts from both models, drawn as in Figure 2: a forecast fan from
# every 4th forecast date within each season, with the final data; (B) WIS of the flatline relative to
# the seasonal model by forecast date (mean WIS over regions and horizons, flatline / seasonal; < 1 =
# flatline better), on a log scale so that "2x better" and "2x worse" are equally far from 1; (C) the
# same by region (mean over horizons), as colour.
# The three panels share one time axis split by season (October-April), so each season lines up
# exactly across A, B and C; seasons are labelled above A and below C.
# Reads:  pipeline/output/scores/ili_asof.parquet, ili_asof_seasonal.parquet (and the quantiles of both)
# Writes: manuscript/output/Figure_4.png and .pdf (number from items.csv)
source("manuscript/_setup.R")

FAN_EVERY  <- 4                       # show the forecast from every 4th forecast date (= every 4 weeks)
STRIP_SIZE <- 12                      # season labels above A and below C
HEIGHTS    <- c(1, 0.7, 1.3)        # relative heights of panels A, B, C
LEGEND_W   <- 0.12                    # width of the legend column (fraction of the figure)
REGIONS    <- c("nat", paste0("hhs", 1:10))   # panel C, top to bottom
season_lab <- function(x) paste0(x, "-", substr(as.integer(x) + 1, 3, 4))   # 2014 -> "2014-15"
FLATLINE   <- "log_sym_wall"          # flatline compared (trend grid model id)
SEASONAL   <- "seasonal_last3y"       # seasonal variant compared
MODEL_LAB  <- c(Flatline = "Flatline", Seasonal = "Seasonal")   # legend labels (models named in the caption)

# Both models on the same forecasts: the wILI analysis set (as in Figure 3), seasonal scores matched to it
cols <- c("location", "reference_date", "horizon", "model", "wis")
trend <- load_scores(ILI_MAIN, models = FLATLINE)[, ..cols]
seas  <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "scores", paste0(ILI_MAIN, "_seasonal.parquet"))))[
  model == SEASONAL & !is.na(wis), ..cols][unique(trend[, .(location, reference_date, horizon)]),
                                           on = .(location, reference_date, horizon), nomatch = 0]
stopifnot(nrow(seas) == nrow(trend))
cmp <- rbind(trend, seas)
message(sprintf("Figure 4: %s vs %s on %d forecasts; overall relative WIS %.3f", FLATLINE, SEASONAL, nrow(trend),
                mean(trend$wis) / mean(seas$wis)))
by_date <- dcast(cmp[, .(wis = mean(wis)), by = .(reference_date, model)], reference_date ~ model, value.var = "wis")
by_date[, `:=`(rel = get(FLATLINE) / get(SEASONAL), season = flu_season(reference_date))]
by_reg <- dcast(cmp[, .(wis = mean(wis)), by = .(reference_date, location, model)],
                reference_date + location ~ model, value.var = "wis")
by_reg[, `:=`(rel = get(FLATLINE) / get(SEASONAL), season = flu_season(reference_date),
              location = factor(location, levels = rev(REGIONS)))]

q_ili <- rbind(
  as.data.table(arrow::read_parquet(file.path(OUT_DIR, "quantiles", paste0(ILI_MAIN, ".parquet"))))[model == FLATLINE],
  as.data.table(arrow::read_parquet(file.path(OUT_DIR, "quantiles", paste0(ILI_MAIN, "_seasonal.parquet"))))[model == SEASONAL],
  fill = TRUE)[location == "nat" & quantile %in% c(0.025, 0.5, 0.975) &
                 reference_date %in% unique(cmp$reference_date)]
# every 4th forecast date within each season
od <- data.table(reference_date = sort(unique(q_ili$reference_date)))
od[, season := flu_season(reference_date)]
q_ili <- q_ili[reference_date %in% od[, .SD[seq(1, .N, by = FAN_EVERY)], by = season]$reference_date]
q_ili <- dcast(q_ili, model + reference_date + horizon + target_end_date ~ quantile, value.var = "value")
setnames(q_ili, c("0.025", "0.5", "0.975"), c("lo", "med", "hi"))
q_ili[, `:=`(Model = factor(ifelse(model == SEASONAL, "Seasonal", "Flatline"), levels = c("Flatline", "Seasonal")),
             season = flu_season(reference_date))]

# One x range per season, shared by all three panels: first forecast date to the last 4-week-ahead
# target. With space = "free_x" the panels then have identical widths in A, B and C.
seasons <- cmp[, .(start = min(reference_date) - 7, end = max(reference_date) + 28), by = .(season = flu_season(reference_date))]
frame <- seasons[, .(date = c(start, end)), by = season]
nat <- load_target(ILI_MAIN)$truth[location == "nat"]
nat <- rbindlist(lapply(seq_len(nrow(seasons)), function(i)
  nat[date >= seasons$start[i] & date <= seasons$end[i]][, season := seasons$season[i]]))

season_facets <- function(strip = "top") facet_grid(~season, scales = "free_x", space = "free_x",
  labeller = as_labeller(season_lab), switch = if (strip == "bottom") "x" else NULL)
no_x <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),
              axis.line.x = element_blank(), panel.spacing.x = unit(0.35, "lines"))
# Relative WIS on a log2 scale for the colours: blue below 1 (flatline better), orange above
REL_BREAKS <- c(0.25, 0.5, 1, 2, 4)
REL_LAB    <- "Relative WIS"
diverging_fill <- scale_fill_gradient2(low = DIVERGING["high"], mid = DIVERGING["mid"], high = DIVERGING["low"],
  midpoint = 0, limits = log2(range(REL_BREAKS)), oob = scales::squish,
  breaks = log2(REL_BREAKS), labels = REL_BREAKS, name = REL_LAB)

pA <- ggplot(q_ili, aes(target_end_date)) +
  geom_blank(data = frame, aes(date)) +
  geom_line(data = nat, aes(date, value), colour = "grey10", linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = Model, group = interaction(Model, reference_date)), alpha = 0.3) +
  geom_line(aes(y = med, colour = Model, group = interaction(Model, reference_date)), linewidth = 0.6) +
  season_facets("top") +
  scale_colour_manual(values = COL[c("Flatline", "Seasonal")], labels = MODEL_LAB, name = NULL) +
  scale_fill_manual(values = COL[c("Flatline", "Seasonal")], labels = MODEL_LAB, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_date(expand = c(0, 0)) +
  labs(y = "wILI % (National)") +
  theme(strip.text = element_text(face = "plain", hjust = 0.5, size = STRIP_SIZE)) + no_x
pB <- ggplot(by_date, aes(fill = log2(rel))) +
  geom_blank(data = frame, aes(x = date), inherit.aes = FALSE) +
  geom_hline(yintercept = 1, colour = "grey40", linewidth = 0.3) +
  # bars from 1 to the relative WIS (geom_col cannot start at 1 on a log axis)
  geom_rect(aes(xmin = reference_date - 3, xmax = reference_date + 3, ymin = pmin(rel, 1), ymax = pmax(rel, 1))) +
  season_facets() + diverging_fill + scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(trans = "log10", breaks = c(0.1, REL_BREAKS), labels = function(x) format(x, drop0trailing = TRUE)) +
  labs(y = REL_LAB) +
  theme(strip.text = element_blank()) + no_x
pC <- ggplot(by_reg, aes(reference_date, location, fill = log2(rel))) +
  geom_blank(data = frame, aes(date), inherit.aes = FALSE) +
  geom_tile() +
  season_facets("bottom") + diverging_fill + scale_x_date(expand = c(0, 0)) +
  labs(y = NULL) +
  theme(strip.text = element_text(face = "plain", hjust = 0.5, size = STRIP_SIZE), strip.placement = "outside",
        legend.position = "right") + no_x

legend_A  <- cowplot::get_plot_component(pA + theme(legend.position = "right"), "guide-box-right", return_all = TRUE)
legend_BC <- cowplot::get_plot_component(pC, "guide-box-right", return_all = TRUE)
panels <- cowplot::plot_grid(pA + theme(legend.position = "none"), pB + theme(legend.position = "none"),
                             pC + theme(legend.position = "none"),
                             ncol = 1, rel_heights = HEIGHTS, align = "v", axis = "lr",
                             labels = c("A", "B", "C"), label_size = 14, hjust = 0, label_x = 0.005)
legends <- cowplot::plot_grid(legend_A, legend_BC, ncol = 1, rel_heights = c(HEIGHTS[1], sum(HEIGHTS[2:3])))
p <- cowplot::plot_grid(panels, legends, nrow = 1, rel_widths = c(1 - LEGEND_W, LEGEND_W))
p
save_fig(p, "seasonal_vs_flatline", 10, 6)
