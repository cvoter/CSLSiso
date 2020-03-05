#' Plot water level gradients and isotope sample dates for wells
#'
#' This function creates a plot object to compare the difference in lake level
#' and well level measurements for all sites at a given lake, paired with when
#' isotope measurements were collected at each site.
#'
#' @param df data frame with the date, site_id, water level difference (diff_m),
#'           and sample result, if exists (d18O).
#' @param date_intervals string with desired break interval for dates, defaults
#'                       to "6 months".
#' @param date_formats string with desired formatting for date labels, defaults
#'                     to "%b '%y" for "Oct '19"-type formatting.
#' @param title string to use for title, defaults to "". Could be
#'              sprintf("%s Lake -  Water Level Comparison", lake)
#' @param text_size size of font, defaults to 10 point

#'
#' @return plot_obj - a plot object with water level difference and dates of
#'                    isotope measurements for each lake site.
#'
#' @import ggplot2
#' @import extrafont
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#'
#' @export
plot_gradients <- function(df,
                           date_intervals = "6 months",
                           date_formats = "%b '%y",
                           title = "",
                           text_size = 10){
  red_points <- df %>%
                filter(!is.na(.data$d18O),
                       !is.na(.data$diff_m))
  grey_points <- df %>%
                 filter(!is.na(.data$d18O),
                        is.na(.data$diff_m)) %>%
                 mutate(diff_m = 0)
  plot_obj <- ggplot() +
              geom_hline(yintercept = 0,
                         linetype = "dashed") +
              geom_line(data = df,
                        aes(x = .data$date,
                            y = .data$diff_m,
                            color = "Water Level Gradient")) +
              geom_point(data = red_points,
                         aes(x = .data$date,
                             y = .data$diff_m,
                             color = "Samples w/Water Levels"),
                         shape = 16,
                         size = 2) +
              scale_color_manual(values = c("darkred", "grey70"),
                                 guide = guide_legend(override.aes = list(
                                           linetype = c("blank", "solid"),
                                           shape = c(16, NA)))) +
              scale_x_datetime(date_breaks = date_intervals,
                               date_labels = date_formats) +
              facet_wrap(~site_id, ncol = 4) +
              labs(x = "",
                   y = "GW Level (m) - Lake Level (m)",
                   title = title,
                   color = "") +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = "top")
  if (nrow(grey_points) > 0) {
    plot_obj <- plot_obj +
                geom_point(data = grey_points,
                           aes(x = .data$date,
                               y = .data$diff_m,
                               color = "Samples w/out Water Levels"),
                           shape = 16,
                           size = 2) +
                 scale_color_manual(values = c("grey40", "darkred", "grey70"),
                                    guide = guide_legend(override.aes = list(
                                      linetype = c("blank", "blank", "solid"),
                                      shape = c(16, 16, NA))))

  }
  return(plot_obj)
}
