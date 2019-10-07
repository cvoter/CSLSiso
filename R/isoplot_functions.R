# isoplot_functions.R
# Functions include;
# - plot_facet
# - plot_colors

# ------------------------------------------------------------------------------
#' Plot d18O
#'
#' This function creates a plot object to evaluate the timeseries of d18O
#' measurements over time at each site.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param lake_isotopes a data frame with the date, lake, site_id, d18O, and d2H of
#'                      precip, groundwater, and lake measurements.
#'
#' @return plot_obj - a plot object with aesthetics added
#'
#' @import ggplot2
#' @import extrafont
#' @import lubridate
#' @importFrom rlang .data
#'
#' @export

plot_d18O <- function(lake, lake_isotopes) {
  plot_obj <- ggplot(data = lake_isotopes,
                     aes(x = .data$date,
                         y = .data$d18O,
                         group = .data$site_id)) +
              geom_point(shape = 16,
                         size = 3,
                         color = "black") +
              facet_wrap(~site_id) +
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
#'
#' @return plot_obj - a plot object with water level difference and dates of
#'                    isotope measurements for each lake site.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom rlang .data
#'
#' @export
plot_levels <- function(lake, water_level_diff, lake_isotopes){
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
              theme(text = element_text(family = "Segoe UI Semilight"),
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
#'
#' @return plot_obj - a plot object with aesthetics added
#'
#' @import ggplot2
#' @import extrafont
#' @import lubridate
#' @importFrom rlang .data
#'
#' @export

plot_iso <- function(lake, lake_isotopes) {
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
#'
#' @return new_obj - a plot object with aesthetics added
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom rlang .data
#'
#' @export

plot_colors <- function(plot_obj) {
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
             theme(text = element_text(family = "Segoe UI Semilight"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())
  return(new_obj)
}
