# isoplot_functions.R
# Functions include:
# - plot_d18O
# - plot_levels
# - plot_iso
# - plot_colors

# ------------------------------------------------------------------------------
#' Plot Balance
#'
#' This function creates a plot object to evaluate the timeseries of d18O
#' measurements over time at each site.
#'
#' @param annual_h2o_bal a data frame with the date and all fluxes into and out
#'                        of the lake.
#' @param as_vol logical defaults to TRUE to plot water balance as a volume
#'               (m^3). If FALSE, plots as depths.
#' @param as_pcnt defaults to FALSE to indicate volumes only. Set to TRUE to
#'                plot fluxes as percent of total in/out fluxes.
#' @param text_size size of font, defaults to 12 point
#'
#' @return plot_obj - a plot object with aesthetics added
#'
#' @import ggplot2
#' @import extrafont
#' @import lubridate
#' @importFrom rlang .data
#' @importFrom reshape2 melt
#'
#' @export

plot_annual_bal <- function(annual_h2o_bal, as_vol = TRUE, as_pcnt = FALSE,
                            text_size = 12) {
  if (as_vol){
    annual_h2o_bal <- annual_h2o_bal %>%
                      select(lake = .data$lake,
                             P = .data$P_m3,
                             E = .data$E_m3,
                             GWin = .data$GWin_m3,
                             GWout = .data$GWout_m3,
                             dV = .data$dV_m3)
    ylabel      <- expression(Volume~(m^{3}))
    label_scale <- scales::scientific
  } else {
    annual_h2o_bal <- annual_h2o_bal %>%
                      select(lake = .data$lake,
                             P = .data$P_mm,
                             E = .data$E_mm,
                             GWin = .data$GWin_mm,
                             GWout = .data$GWout_mm,
                             dV = .data$dV_mm)
    ylabel      <- "Flux (mm)"
    label_scale <- scales::number
  }

  melted_bal <- melt(annual_h2o_bal, id.vars = "lake")

  for (i in 1:nrow(melted_bal)) {
    if (melted_bal$variable[i] == "P" |
        melted_bal$variable[i] == "GWin") {
      melted_bal$in_or_out[i] <- "In"
    } else {
      melted_bal$in_or_out[i] <- "Out"
    }
  }

  if (as_pcnt){
    melted_bal <- melted_bal %>%
                  group_by(.data$lake) %>%
                  mutate(value = 100*.data$value/
                                 sum(.data$value[.data$in_or_out == "In"]))
    ylabel      <- "Flux (%)"
    label_scale <- scales::number
  }

  plot_obj <- ggplot(data = melted_bal) +
              geom_col(aes(x = .data$in_or_out,
                           y = .data$value,
                           fill = .data$variable)) +
              facet_wrap(~.data$lake) +
              scale_y_continuous(expand = c(0,0),
                                 labels = label_scale) +
              scale_fill_manual(name = "",
                                breaks = c("P","E","GWin","GWout","dV"),
                                labels = c("Precipitation",
                                           "Evaporation",
                                           "GW Inflow",
                                           "GW Outflow",
                                           expression(paste(Delta," Lake Volume"))),
                                values = c("#1F78B4", "#A6CEE3",
                                           "#33A02C", "#B2DF8A",
                                           "#FB9A99")) +
              labs(x = "", y = ylabel, title = "All Lakes - Annual Balance") +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    legend.text.align = 0,
                    legend.position = "top")

  return(plot_obj)
}

# ------------------------------------------------------------------------------
#' Plot Balance
#'
#' This function creates a plot object to evaluate the timeseries of d18O
#' measurements over time at each site.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param monthly_h2o_bal a data frame with the date and all fluxes into and out
#'                        of the lake.
#' @param as_vol logical defaults to TRUE to plot water balance as a volume
#'               (m^3). If FALSE, plots as depths.
#' @param annual defaults to FALSE to indicate monthly timestep. Set to TRUE to
#'               indicate annual balance.
#' @param text_size size of font, defaults to 12 point
#'
#' @return plot_obj - a plot object with aesthetics added
#'
#' @import ggplot2
#' @import extrafont
#' @import lubridate
#' @importFrom rlang .data
#' @importFrom reshape2 melt
#' @importFrom stats reorder
#'
#' @export

plot_balance <- function(lake, monthly_h2o_bal, as_vol = TRUE, annual = FALSE,
                         text_size = 12) {
  monthly_h2o_bal <- monthly_h2o_bal %>%
                     filter(is.na(.data$GWout_m3) == FALSE)
  if (as_vol){
    monthly_h2o_bal <- monthly_h2o_bal %>%
                       select(date = .data$date,
                              P = .data$P_m3,
                              E = .data$E_m3,
                              GWin = .data$GWin_m3,
                              GWout = .data$GWout_m3,
                              dV = .data$dV_m3)
    ylabel <- expression(Volume~(m^{3}))
    label_scale <- scales::scientific
  } else {
    monthly_h2o_bal <- monthly_h2o_bal %>%
                       select(data = .data$date,
                              P = .data$P_mm,
                              E = .data$E_mm,
                              GWin = .data$GWin_mm,
                              GWout = .data$GWout_mm,
                              dV = .data$dV_mm)
    ylabel <- "Flux (mm)"
    label_scale <- scales::number
  }

  if (annual) {
    monthly_h2o_bal$date <- as_datetime(mdy("9/1/18"))
    plot_title <- sprintf("%s Lake - Annual Balance", lake)
  } else {
    plot_title <- sprintf("%s Lake - Monthly Balance", lake)
  }

  melted_bal <- melt(monthly_h2o_bal, id.vars = "date")

  for (i in 1:nrow(melted_bal)) {
    if (melted_bal$variable[i] == "P" |
        melted_bal$variable[i] == "GWin") {
      melted_bal$in_or_out[i] <- "In"
    } else {
      melted_bal$in_or_out[i] <- "Out"
    }
  }

  melted_bal$date_labs <- format(melted_bal$date, "%b %Y")
  melted_bal$date_labs <- reorder(melted_bal$date_labs, melted_bal$date)

  plot_obj <- ggplot(data = melted_bal) +
              geom_col(aes(x = .data$in_or_out,
                           y = .data$value,
                           fill = .data$variable)) +
              facet_wrap(~.data$date_labs,
                         ncol = 4) +
              scale_y_continuous(expand = c(0,0),
                                 labels = label_scale) +
              scale_fill_manual(name = "",
                      breaks = c("P","E","GWin","GWout","dV"),
                      labels = c("Precipitation",
                                 "Evaporation",
                                 "GW Inflow",
                                 "GW Outflow",
                                 expression(paste(Delta," Lake Volume"))),
                      values = c("#1F78B4", "#A6CEE3",
                                 "#33A02C", "#B2DF8A",
                                 "#FB9A99")) +
             labs(x = "", y = ylabel, title = plot_title) +
             theme_bw() +
             theme(text = element_text(family = "Segoe UI Semilight",
                                       size = text_size),
                   plot.title = element_text(hjust = 0.5),
                   legend.text.align = 0,
                   legend.position = "top")

    return(plot_obj)
}


# ------------------------------------------------------------------------------
#' Plot d18O
#'
#' This function creates a plot object to evaluate the timeseries of d18O
#' measurements over time at each site.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param lake_isotopes a data frame with the date, lake, site_id, d18O, and d2H of
#'                      precip, groundwater, and lake measurements.
#' @param text_size size of font, defaults to 12 point
#'
#' @return plot_obj - a plot object with aesthetics added
#'
#' @import ggplot2
#' @import extrafont
#' @import lubridate
#' @importFrom rlang .data
#'
#' @export

plot_d18O <- function(lake, lake_isotopes, text_size = 12) {
  plot_obj <- ggplot(data = lake_isotopes,
                     aes(x = as.Date(.data$date),
                         y = .data$d18O,
                         group = .data$site_id)) +
              geom_point(shape = 16,
                         size = 3,
                         color = "black") +
              facet_wrap(~site_id) +
              scale_x_date(date_breaks = "4 months",
                           date_labels = "%b %y") +
              labs(x = "",
                   y = "d18O",
                   title = sprintf("%s Lake - d18O", lake)) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight"),
                    legend.position = "top")
  return(plot_obj)
}


# ------------------------------------------------------------------------------
#' Plot Levels
#'
#' This function creates a plot object to compare the difference in lake level
#' and well level measurements for all sites at a given lake, paired with when
#' isotope measurements were collected and what the d18O results were.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param water_level_diff a data frame with the date, site_no, obs_type,
#'                     level_m (of gw levels), and diff_m between daily gw level
#'                     observations and lake level observations.
#' @param lake_isotopes a data frame with the date, lake, site_id, d18O, and d2H of
#'                      precip, groundwater, and lake measurements.
#' @param text_size size of font, defaults to 12 point
#'
#' @return plot_obj - a plot object with water level difference and dates of
#'                    isotope measurements for each lake site.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom rlang .data
#'
#' @export
plot_levels <- function(lake, water_level_diff, lake_isotopes, text_size = 12){
  plot_obj <- ggplot() +
              geom_hline(yintercept = 0,
                         linetype = "dashed") +
              geom_line(data = water_level_diff,
                        aes(x = .data$date,
                            y = .data$diff_m)) +
              geom_point(data = lake_isotopes,
                         aes(x = .data$date,
                             y = .data$diff_m,
                             fill = .data$d18O),
                         shape = 21,
                         size = 3,
                         color = "black") +
              scale_fill_distiller(name = "d18O",
                                   palette = "RdYlBu",
                                   direction = 1) +
              facet_wrap(~site_id) +
              labs(x = "",
                   y = "GW Level (m) - Lake Level (m)",
                   title = sprintf("%s Lake -  Water Level Comparison", lake)) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = "top")
  return(plot_obj)
}

# ------------------------------------------------------------------------------
#' Plot Isotope Relationships
#'
#' This function contains the all aesthetics for plotting the isotopic
#' signatures of all measured wells at a lake with each site faceted out into
#' its own plot
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param lake_isotopes a data frame with the date, lake, site_id, d18O, and d2H of
#'                      precip, groundwater, and lake measurements.
#' @param text_size size of font, defaults to 12 point
#'
#' @return plot_obj - a plot object with aesthetics added
#'
#' @import ggplot2
#' @import extrafont
#' @import lubridate
#' @importFrom rlang .data
#'
#' @export

plot_iso <- function(lake, lake_isotopes, text_size = 12) {
  start_date    <- floor_date(min(lake_isotopes$date), unit = "month")
  end_date      <- ceiling_date(max(lake_isotopes$date), unit = "month")
  date_interval <- interval(start = start_date, end = end_date)
  nmonths       <- round(time_length(date_interval, unit = "month"))
  quarter       <- round(nmonths/4)
  label1        <- start_date + months(round(quarter/2))
  label2        <- start_date + months(round(nmonths/2))
  label3        <- end_date - months(round(quarter/2))

  precip <- lake_isotopes %>%
            filter(.data$site_id == "PRECIP") %>%
            select(.data$d18O, .data$d2H)

  plot_obj <- ggplot(data = lake_isotopes,
                    aes(x = .data$d18O, y = .data$d2H)) +
             geom_abline(slope =  7.8,
                         intercept = 12.7,
                         linetype = "dashed",
                         size = 1) +
             geom_smooth(data = precip,
                         aes(x = .data$d18O, y = .data$d2H),
                         method = "lm",
                         se = FALSE,
                         size = 1,
                         color = "darkred") +
             geom_path(aes(group = .data$site_id),
                       color = "black") +
             geom_point(aes(group = .data$site_id,
                            fill = as.numeric(date, origin = "1970-01-01")),
                        shape = 21,
                        size = 3,
                        color = "black") +
             facet_wrap(~site_id) +
             labs(x = "d18O",
                  y = "d2H",
                  title = sprintf("%s Lake - LMWL Comparison", lake)) +
             scale_fill_distiller(name = "Date",
                                  palette = "Greys",
                                  direction = 1,
                                  breaks = as.numeric(as.Date(c(label1,
                                                                label2,
                                                                label3))),
                                  labels = format(c(label1,
                                                    label2,
                                                    label3), "%b '%y")) +
             theme_bw() +
             theme(text = element_text(family = "Segoe UI Semilight"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   legend.position = "top")
  return(plot_obj)
}
# ------------------------------------------------------------------------------
#' Plot Colors
#'
#' This function contains the all aesthetics for plotting the isotopic
#' signatures of all measured wells at a lake with all sites on the same plot,
#' denoted by color
#'
#' @param plot_obj  a plot object created with ggplot(data = actual_data, aes(x =
#'                 actual_x, y = actual_y, group = actual_group))
#' @param text_size size of font, defaults to 12 point
#'
#' @return new_obj - a plot object with aesthetics added
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom rlang .data
#'
#' @export

plot_colors <- function(plot_obj, text_size = 12) {
  new_obj <- plot_obj +
             geom_abline(slope =  7.8,
                         intercept = 12.7,
                         color = "darkred",
                         size = 2) +
             geom_path() +
             geom_point(aes(fill = .data$site_id),
                        shape = 21,
                        size = 3,
                        color = "black") +
             labs(fill = "Site ID") +
             theme_bw() +
             theme(text = element_text(family = "Segoe UI Semilight",
                                       size = text_size),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())
  return(new_obj)
}
