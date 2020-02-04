#' Plot water balance inputs for one or more lakes
#'
#' This function creates a plot object that showcases a given input over time
#' for each lake of interest.
#'
#' @param df optional data frame passed to function containing columns "date",
#'           ydata, and "lake". Avoids need to run
#'           \code{\link{runall_csls_inputs}} or
#'           \code{\link{runall_csls_budget}} as part of this function.
#' @param ydata colname of input data to display, e.g., "dV_m3" or "d18O_evap"
#' @param ylabel string to display for y-axis title, e.g.,
#'               "expression(Change~Lake~Volume~(m^{3}))" or
#'               "expression(delta^18*'O')"
#' @param yscales type of scaling for y-axis, defaults to scales::scientific.
#'                Other options include scales::number for decimal format,
#'                scales::percent for percentages, etc.
#' @param geom_type defaults to "col" to dispay as bar plot. Set to "point" to
#'                  instead display as points, or "line" for lines.
#' @param title string to display for title, defaults to "".
#' @param facet_scales type of scaling of axis for facets, defaults to "fixed".
#'                     Can also be "free_y", "free_x", or "free".
#' @param lakes names of lakes to plot, defaults to include all:
#'              "c("Pleasant", "Long", "Plainfield")"
#' @param inputs_only logical defaults to TRUE to trigger
#'                    \code{\link{runall_csls_inputs}}. Set to FALSE to trigger
#'                    \code{\link{runall_csls_budget}} instead (i.e., to obtain
#'                    groundwater inflow or outflow values).
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

plot_input <- function(df = NULL, ydata, ylabel, yscales = scales::scientific,
                       geom_type = "col", title = "", facet_scales = "fixed",
                       lakes = c("Pleasant", "Long", "Plainfield"),
                       inputs_only = TRUE, date_intervals = "6 months",
                       date_formats = "%b '%y", text_size = 10){
  # Get desired inputs for desired lakes in one data frame
  if (is.null(df)) {
    for (lake in lakes){
      if (inputs_only){
        tmp         <- runall_csls_inputs(lake)
      } else {
        tmp         <- runall_csls_budget(lake)
      }
      tmp           <- tmp[,c("date", ydata)]
      colnames(tmp) <- c("x", "y")
      tmp$lake      <- lake
      df            <- rbind(df, tmp)
    }
    df$lake         <- factor(df$lake, levels = lakes)
  } else {
    df              <- df[,c("date", ydata, "lake")]
    colnames(df)    <- c("x", "y", "lake")
  }

  # Specify plot type (column or point)
  if (geom_type == "col") {
    plot_obj <- ggplot(df, aes(x = .data$x, y = .data$y)) + geom_col()
  } else if (geom_type == "point") {
    plot_obj <- ggplot(df, aes(x = .data$x, y = .data$y)) + geom_point()
  } else if (geom_type == "line") {
    plot_obj <- ggplot(df, aes(x = .data$x, y = .data$y)) + geom_line()
  }

  # Add in other plot aesthetics
  plot_obj <- plot_obj +
              scale_x_datetime(date_breaks = date_intervals,
                               date_labels = date_formats) +
              facet_wrap(~lake, scales = facet_scales) +
              scale_y_continuous(labels = yscales) +
              labs(x = "", y = ylabel, title = title) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

  return(plot_obj)
}
