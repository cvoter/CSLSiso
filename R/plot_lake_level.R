#' Plot lake levels for one or more lakes
#'
#' This function creates a plot object that showcases lake levels (as levels,
#' areas, or volumes) over time for each lake of interest.
#'
#' @param ydata colname of input data to display, defaults to "level_m" but can
#'              also be "area_m2" or "vol_m3"
#' @param ylabel string to display for y-axis title, e.g., "Lake Elevation (m)"
#'               or "expression(Lake~Volume~(m^{3}))"
#' @param yscales type of scaling for y-axis, defaults to scales::number.
#'                Other options include scales::scientific for scientific notation
#' @param title string to display for title, defaults to "".
#' @param lakes names of lakes to plot, defaults to include all:
#'              "c("Pleasant", "Long", "Plainfield")"
#' @param date_intervals string with desired break interval for dates, defaults
#'                       to "6 months".
#' @param date_formats string with desired formatting for date labels, defaults
#'                     to "%b '%y" for "Oct '19"-type formatting.
#' @param text_size size of text, defaults to 10 pt.
#'
#' @return plot_obj - a plot object with aesthetics added
#'
#' @import ggplot2
#' @import extrafont
#' @import lubridate
#' @importFrom rlang .data
#'
#' @export

plot_lake_level <- function(ydata = "level_m",
                            ylabel = "Lake Elevation (m)",
                            yscales = scales::number,
                            title = "",
                            lakes = c("Pleasant", "Long", "Plainfield"),
                            date_intervals = "6 months",
                            date_formats = "%b '%y",
                            text_size = 10){

  # Get lake levels for desired lakes in one data frame
  lake_levels <- NULL
  for (lake in lakes){
    tmp           <- CSLSdata::lake_levels[[lake]]
    tmp           <- tmp[c("date", ydata)]
    colnames(tmp) <- c("x", "y")
    tmp$lake      <- lake
    lake_levels   <- rbind(lake_levels, tmp)
  }
  lake_levels$lake <- factor(lake_levels$lake, levels = lakes)

  # Plot lake levels
  plot_obj <- ggplot(lake_levels) +
              geom_line(aes(x = .data$x, y = .data$y)) +
              scale_x_datetime(date_breaks = date_intervals,
                               date_labels = date_formats) +
              scale_y_continuous(labels = ) +
              labs(x = "", y = ylabel, title = title) +
              facet_wrap(~lake, scales = "free_y") +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

  return(plot_obj)
}

