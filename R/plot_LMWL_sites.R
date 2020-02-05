#' Plot isotope relationships by site
#'
#' This function contains the all aesthetics for plotting the isotopic
#' signatures of all measured wells at a lake with each site faceted out into
#' its own plot
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
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

plot_LMWL_sites <- function(lake, text_size = 12) {
  isotopes      <- CSLSdata::isotopes[[lake]]
  start_date    <- floor_date(min(isotopes$date), unit = "month")
  end_date      <- ceiling_date(max(isotopes$date), unit = "month")
  date_interval <- interval(start = start_date, end = end_date)
  nmonths       <- round(time_length(date_interval, unit = "month"))
  quarter       <- round(nmonths/4)
  label1        <- start_date + months(round(quarter/2))
  label2        <- start_date + months(round(nmonths/2))
  label3        <- end_date - months(round(quarter/2))

  precip <- isotopes %>%
            filter(.data$site_id == "PRECIP") %>%
            select(.data$d18O, .data$d2H)
  plot_isotopes <- isotopes %>%
                   filter(!.data$site_id %in% c("PLEAS", "LONG", "PLAIN", "PRECIP"))

  plot_obj <- ggplot(data = plot_isotopes,
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
              facet_wrap(~site_id, ncol = 4) +
              labs(x = expression(delta^18*'O'),
                   y = expression(delta^2*'H'),
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
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = "top")
  return(plot_obj)
}
