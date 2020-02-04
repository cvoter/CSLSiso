#' Plot isotope relationships
#'
#' This function contains the all aesthetics for plotting the isotopic
#' signatures of precipitation, groundwater, and lake water based on all
#' samples. Also displays the Local Meteoric Water Line and Local Evaporation
#' Water Line. Options to facet by lake (default) or plot all on one graph. Can
#' turn of groundwater samples individually if desired.
#'
#' @param lakes lakes of interest, defaults to
#'              c("Pleasant", "Long", "Plainfield")
#' @param site_types type of sites, defaults to c("precipitation", "upgradient",
#'                   "downgradient", "deep", "lake")
#' @param facets_on defaults to TRUE to facet results by lake
#' @param upgradient_off logical defaults to FALSE to include upgradient wells
#' @param downgradient_off logical defaults to FALSE to include downgradient wells
#' @param deep_off logical defaults to FALSE to include deep wells
#' @param text_size size of font, defaults to 12 point
#'
#' @return plot_obj - a plot object with aesthetics added
#'
#' @import ggplot2
#' @import extrafont
#' @import lubridate
#' @importFrom rlang .data
#' @importFrom stringr str_to_sentence
#' @importFrom dplyr filter select
#' @importFrom magrittr %>%
#'
#' @export

plot_LMWL_all <- function(lakes = c("Pleasant", "Long", "Plainfield"),
                          site_types = c("precipitation",
                                         "upgradient",
                                         "downgradient",
                                         "deep",
                                         "lake"),
                          facets_on = TRUE,
                          upgradient_off = FALSE,
                          downgradient_off = FALSE,
                          deep_off = FALSE,
                          text_size = 12) {
  isotopes <- NULL
  for (lake in lakes){
    these_isotopes <- CSLSdata::isotopes[[lake]]
    dictionary     <- CSLSdata::dictionary[[lake]]
    lake_levels    <- CSLSdata::lake_levels[[lake]]
    gw_levels      <- CSLSdata::gw_levels[[lake]]
    these_isotopes <- iso_site_type(these_isotopes, dictionary,
                                    lake_levels, gw_levels)
    these_isotopes$lake <- lake
    isotopes       <- rbind(isotopes, these_isotopes)
  }
  isotopes$lake      <- factor(isotopes$lake, levels = lakes)
  isotopes$site_type <- factor(isotopes$site_type, levels = site_types)

  precip <- isotopes %>%
            filter(.data$site_type == "precipitation") %>%
            select(.data$d18O, .data$d2H)

  lakes  <- isotopes %>%
            filter(.data$site_type == "lake") %>%
            select(.data$d18O, .data$d2H)

  plot_isotopes   <- isotopes %>% filter(!is.na(.data$site_type))
  if (upgradient_off) {
    plot_isotopes <- plot_isotopes %>% filter(.data$site_type != "upgradient")
  }
  if (downgradient_off) {
    plot_isotopes <- plot_isotopes %>% filter(.data$site_type != "downgradient")
  }
  if (deep_off) {
    plot_isotopes <- plot_isotopes %>% filter(.data$site_type != "deep")
  }

  plot_obj <- ggplot(data = plot_isotopes,
                     aes(x = .data$d18O,
                         y = .data$d2H)) +
              geom_smooth(data = precip,
                          aes(x = .data$d18O, y = .data$d2H, color = "LMWL"),
                          method = "lm",
                          se = FALSE,
                          size = 1) +
              geom_smooth(data = lakes,
                          aes(x = .data$d18O, y = .data$d2H, color = "LEWL"),
                          method = "lm",
                          se = FALSE,
                          size = 1) +
              geom_point(aes(fill = .data$site_type,
                             shape = .data$site_type),
                         size = 2.5,
                         color = "black") +
              labs(x = expression(delta^18*'O'),
                   y = expression(delta^2*'H'),
                   title = "") +
              scale_fill_manual(name = "Site Type",
                                breaks = site_types,
                                labels = str_to_sentence(site_types),
                                values = c("#1F78B4", #dark blue
                                           "#33A02C", #dark green
                                           "#B2DF8A", #light green
                                           "#6A3D9A", #purple
                                           "#FB9A99")) + #pink
              scale_shape_manual(name = "Site Type",
                                 breaks = site_types,
                                 labels = str_to_sentence(site_types),
                                 values = c(22, 21, 21, 21, 24)) +
              scale_color_manual(name = "Local Water Lines",
                                 breaks = c("LMWL", "LEWL"),
                                 values = c("grey70", "black")) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = "top",
                    legend.box = "vertical",
                    legend.box.margin = margin(-0.1,0,0,0),
                    legend.margin = margin(0, 0, 0, 0))

  if (facets_on) {
    plot_obj <- plot_obj + facet_wrap(~lake)
  }

  return(plot_obj)
}
