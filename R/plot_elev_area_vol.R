#' Plot lake elevation vs. area or volume for one or more lakes
#'
#' This function creates a plot object that showcases the lake elevation vs.
#' lake area or lake elevation vs. lake volume relationship for the lakes of
#' interest.
#'
#' @param ydata colname of input data to display, defaults to "level_m" but can
#'              also be "area_m2" or "vol_m3"
#' @param ylabel string to display for y-axis title, e.g., "Lake Elevation (m)"
#'               or "expression(Lake~Volume~(m^{3}))"
#' @param yscales type of scaling for y-axis, defaults to scales::scientific
#'                Other options include scales::number for decimal notation
#' @param title string to display for title, defaults to "".
#' @param lakes names of lakes to plot, defaults to include all:
#'              "c("Pleasant", "Long", "Plainfield")"
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

plot_elev_area_vol <- function(ydata, ylabel, yscales = scales::scientific,
                               title = "",
                               lakes = c("Pleasant", "Long", "Plainfield"),
                               text_size = 10){
  # Get lake elev, area, vol for desired lakes in one data frame
  elev_area_vol <- NULL
  for (lake in lakes){
    tmp              <- CSLSdata::elev_area_vol[[lake]]
    tmp              <- tmp[c("elev_m", ydata)]
    colnames(tmp)    <- c("x", "y")
    tmp$lake         <- lake
    elev_area_vol    <- rbind(elev_area_vol, tmp)
  }
  elev_area_vol$lake <- factor(elev_area_vol$lake, levels = lakes)

  # Creat plot
  plot_obj <- ggplot(elev_area_vol) +
              geom_line(aes(x = .data$x, y = .data$y)) +
              facet_wrap(~lake, scales = "free") +
              scale_y_continuous(labels = yscales) +
              labs(x = "Elevation (m)", y = ylabel, title = title) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

  return(plot_obj)
}

