#' Plot Balance
#'
#' This function creates a plot object to evaluate the timeseries of d18O
#' measurements over time at each site.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param monthly_h2o_bal a data frame with the date and all fluxes into and out
#'                        of the lake.
#' @param as_vol logical defaults to TRUE to plot water balance as a volume
#'               (m^3). If FALSE, plots as depths.
#' @param annual defaults to FALSE to indicate monthly timestep. Set to TRUE to
#'               indicate annual balance.
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

plot_balance <- function(lake, monthly_h2o_bal, as_vol = TRUE, annual = FALSE,
                         text_size = 12) {
  monthly_h2o_bal <- monthly_h2o_bal %>%
    filter(is.na(.data$GWout_m3) == FALSE)
  if (as_vol){
    monthly_h2o_bal <- monthly_h2o_bal %>%
      select(date = .data$date,
             P = .data$P_m3,
             E = .data$E_m3,
             GWin = .data$GWin_m3,
             GWout = .data$GWout_m3,
             dV = .data$dV_m3)
    ylabel <- expression(Volume~(m^{3}))
    label_scale <- scales::scientific
  } else {
    monthly_h2o_bal <- monthly_h2o_bal %>%
      select(data = .data$date,
             P = .data$P_mm,
             E = .data$E_mm,
             GWin = .data$GWin_mm,
             GWout = .data$GWout_mm,
             dV = .data$dV_mm)
    ylabel <- "Flux (mm)"
    label_scale <- scales::number
  }

  if (annual) {
    monthly_h2o_bal$date <- as_datetime(mdy("9/1/18"))
    plot_title <- sprintf("%s Lake - Annual Balance", lake)
  } else {
    plot_title <- sprintf("%s Lake - Monthly Balance", lake)
  }

  melted_bal <- melt(monthly_h2o_bal, id.vars = "date")

  for (i in 1:nrow(melted_bal)) {
    if (melted_bal$variable[i] == "P" |
        melted_bal$variable[i] == "GWin") {
      melted_bal$in_or_out[i] <- "In"
    } else {
      melted_bal$in_or_out[i] <- "Out"
    }
  }

  melted_bal$date_labs <- format(melted_bal$date, "%b %Y")
  melted_bal$date_labs <- reorder(melted_bal$date_labs, melted_bal$date)

  plot_obj <- ggplot(data = melted_bal) +
    geom_col(aes(x = .data$in_or_out,
                 y = .data$value,
                 fill = .data$variable)) +
    facet_wrap(~.data$date_labs,
               ncol = 4) +
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
    labs(x = "", y = ylabel, title = plot_title) +
    theme_bw() +
    theme(text = element_text(family = "Segoe UI Semilight",
                              size = text_size),
          plot.title = element_text(hjust = 0.5),
          legend.text.align = 0,
          legend.position = "top")

  return(plot_obj)
}
