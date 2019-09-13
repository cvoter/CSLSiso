# isoplot_functions.R
# Functions include;
# - plot_facet
# - plot_colors

# ------------------------------------------------------------------------------
#' Plot Facet
#'
#' This function contains the all aesthetics for plotting the isotopic
#' signatures of all measured wells at a lake with each site faceted out into
#' its own plot
#'
#' @param plot_obj a plot object created with ggplot(data = actual_data, aes(x =
#'                 actual_x, y = actual_y, group = actual_group))
#'
#' @return new_obj - a plot object with aesthetics added
#'
#' @import ggplot2
#' @import extrafont
#' @import lubridate
#'
#' @export

plot_facet <- function(plot_obj, df) {
  start_date    <- floor_date(min(df$date), unit = "month")
  end_date      <- ceiling_date(max(df$date), unit = "month")
  date_interval <- interval(start = start_date, end = end_date)
  nmonths       <- round(time_length(date_interval, unit = "month"))
  quarter       <- round(nmonths/4)
  label1        <- start_date + months(round(quarter/2))
  label2        <- start_date + months(round(nmonths/2))
  label3        <- end_date - months(round(quarter/2))


  new_obj <- plot_obj +
             geom_abline(slope =  7.8,
                         intercept = 12.7,
                         color = "darkred",
                         size = 1.5) +
             geom_path(color = "black") +
             geom_point(aes(fill = as.numeric(date, origin = "1970-01-01")),
                        shape = 21,
                        size = 3,
                        color = "black") +
             facet_wrap(~site_id) +
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
  return(new_obj)
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
#'
#' @export

plot_colors <- function(plot_obj) {
  new_obj <- plot_obj +
             geom_abline(slope =  7.8,
                         intercept = 12.7,
                         color = "darkred",
                         size = 2) +
             geom_path() +
             geom_point(aes(fill = site_id),
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
