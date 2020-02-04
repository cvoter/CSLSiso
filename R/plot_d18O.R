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
