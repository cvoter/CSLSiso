#' Plot Balance
#'
#' This function creates a plot object to evaluate the timeseries of d18O
#' measurements over time at each site.
#'
#' @param df a data frame with all fluxes into and out of a lake or lakes. Can
#'           be either monthly values for one lake or annual values for multiple
#'           lakes.
#' @param annual logical defaults to FALSE to create plot for monthly water
#'               budgets. If TRUE, creates plot for annual water budgets.
#' @param as_pcnt logical defaults to FALSE to display as percents.
#' @param text_size size of font, defaults to 12 point
#' @param title string to use for title, defaults to "".
#' @param fill_breaks vector with column names of variables to plot
#' @param fill_labels vector with display labels for plotted variables
#' @param fill_values vector with hex codes for colors associated with each
#'                    variable.
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

plot_budget <- function(df,
                        annual = FALSE,
                        as_pcnt = FALSE,
                        gw_only = FALSE,
                        text_size = 12,
                        title = "",
                        fill_breaks = c("P","E","GWin","GWout","dV"),
                        fill_labels = c("Precipitation",
                                        "Evaporation",
                                        "GW Inflow",
                                        "GW Outflow",
                                        expression(paste(Delta," Lake Volume"))),
                        fill_values = c("#1F78B4",
                                        "#A6CEE3",
                                        "#33A02C",
                                        "#B2DF8A",
                                        "#FB9A99")) {
  # Subset data to plot
  if (annual) {
    df <- df %>%
          select(facet_var = .data$lake,
                 P = .data$P_m3,
                 E = .data$E_m3,
                 GWin = .data$GWin_m3,
                 GWout = .data$GWout_m3,
                 dV = .data$dV_m3)
  } else {
    df <- df %>%
          filter(!is.na(.data$GWin_m3)) %>%
          select(facet_var = .data$date,
                 P = .data$P_m3,
                 E = .data$E_m3,
                 GWin = .data$GWin_m3,
                 GWout = .data$GWout_m3,
                 dV = .data$dV_m3)
    df$tmp       <- format(df$facet_var, "%b %Y")
    df$tmp       <- reorder(df$tmp, df$facet_var)
    df$facet_var <- df$tmp
    df$tmp       <- NULL
  }

  # Group by In vs. Out fluxes
  melted_df <- melt(df, id.vars = "facet_var")
  for (i in 1:nrow(melted_df)) {
    if (melted_df$variable[i] == "P" | melted_df$variable[i] == "GWin") {
      melted_df$group[i] <- "In"
    } else {
      melted_df$group[i] <- "Out"
    }
  }
  melted_df$variable <- factor(melted_df$variable, levels = fill_breaks)

  # Convert to percent, if desired
  if (as_pcnt) {
    melted_df <- melted_df %>%
                 group_by(.data$facet_var) %>%
                 mutate(value = .data$value/
                          sum(.data$value[.data$group == "In"])) %>%
                 ungroup()
    ylabel    <- "Volume (%)"
    yscales   <- scales::percent
  } else {
    ylabel  <- expression(Volume~(m^{3}))
    yscales <- scales::scientific
  }

  if (gw_only){
    melted_df <- melted_df %>%
                 filter(.data$variable %in% c("GWin", "GWout"))
    fill_ids <- which(fill_breaks %in% c("GWin", "GWout"))
    fill_breaks <- fill_breaks[fill_ids]
    fill_labels <- fill_labels[fill_ids]
    fill_values <- fill_values[fill_ids]
    melted_df$group <- suppressWarnings(as_datetime(myd(paste0(melted_df$facet_var, " 01"))))
  }

  # Create plot object
  plot_obj <- ggplot(data = melted_df,
                     aes(x = .data$group,
                         y = .data$value,
                         fill = .data$variable))

  # Decide which type of bar plot (col or bar)
  if (gw_only) {
    plot_obj <- plot_obj +
                geom_bar(stat = "identity", position = 'dodge')
  } else {
    plot_obj <- plot_obj +
                geom_col() +
                facet_wrap(~.data$facet_var)
  }

  # Add aesthetics
  plot_obj <- plot_obj +
              scale_y_continuous(expand = c(0,0),
                                 labels = yscales) +
              scale_fill_manual(name = "",
                                breaks = fill_breaks,
                                labels = fill_labels,
                                values = fill_values) +
              labs(x = "",
                   y = ylabel,
                   title = title) +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight",
                                        size = text_size),
                    plot.title = element_text(hjust = 0.5),
                    legend.text.align = 0,
                    legend.position = "top")
  return(plot_obj)
}
