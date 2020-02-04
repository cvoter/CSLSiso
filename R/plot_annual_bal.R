#' Plot Balance
#'
#' This function creates a plot object to evaluate the timeseries of d18O
#' measurements over time at each site.
#'
#' @param annual_h2o_bal a data frame with the date and all fluxes into and out
#'                        of the lake.
#' @param as_vol logical defaults to TRUE to plot water balance as a volume
#'               (m^3). If FALSE, plots as depths.
#' @param as_pcnt logical defaults to FALSE to display as percents.
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
