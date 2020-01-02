#' Monthly Isotope Measurements
#'
#' Summarizes isotope measurements at a monthly timestep for a single lake
#'
#' @inheritParams summarise_inputs
#' @param timeseries a list with dates (POSIXct) to analyze, and intervals
#'                 (lubridate interval) covering the month before each analysis
#'                 date.
#'
#' @return monthly_isotopes, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{d18O_pcpn}{mean precipitation stable isotope measurement for the month}
#' \item{d18O_lake}{mean lake stable isotope measurement for the month}
#' \item{d18O_GWin}{mean groundwater inflow stable isotope measurement for the
#'                  month}
#' \item{d18O_GWout}{mean groundwater outflow stable isotope measurement for the
#'                  month}
#' \item{GWin_sites}{well sites used in mean d18O_GWin each month}
#' \item{GWout_sites}{well sites used in mean d18O_GWout each month}
#' }
#'
#' @importFrom utils data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select group_by summarise ungroup
#' @importFrom reshape2 recast
#' @importFrom rlang .data
#' @import lubridate
#' @importFrom stringr str_replace str_c
#'
#' @export
summarise_isotopes <- function(isotopes, dictionary, timeseries, lake_levels,
                               gw_levels, threshold = 0.01) {
  # Assign site type to each measurement
  isotopes <- iso_site_type(isotopes, dictionary, lake_levels, gw_levels,
                            threshold)

  # Mean d180 by month and site type, drop values w/out site type
  isomelt <- isotopes %>%
             group_by(floor_date(.data$date, unit = "month"),
                      site_type = .data$site_type) %>%
             filter(is.na(.data$site_type) == FALSE &
                      .data$site_type != "") %>%
             summarise(date = floor_date(mean(.data$date), unit = "day"),
                       mean_d18O = mean(.data$d18O),
                       mean_d2H = mean(.data$d2H)) %>%
             ungroup() %>%
             select(.data$date, .data$site_type,
                    .data$mean_d18O, .data$mean_d2H)

  # Reshape and rename columns
  monthly_isotopes           <- recast(isomelt,
                                       date ~ site_type + variable,
                                       id.var = c("date", "site_type"))
  colnames(monthly_isotopes) <- colnames(monthly_isotopes) %>%
                                str_replace("downgradient_mean_d18O","d18O_GWout") %>%
                                str_replace("upgradient_mean_d18O","d18O_GWin") %>%
                                str_replace("precipitation_mean_d18O","d18O_pcpn") %>%
                                str_replace("lake_mean_d18O","d18O_lake") %>%
                                str_replace("downgradient_mean_d2H","d2H_GWout") %>%
                                str_replace("upgradient_mean_d2H","d2H_GWin") %>%
                                str_replace("precipitation_mean_d2H","d2H_pcpn") %>%
                                str_replace("lake_mean_d2H","d2H_lake")
  if (!"d2H_GWout" %in% colnames(monthly_isotopes)) {
    monthly_isotopes$d18O_GWout <- NA
    monthly_isotopes$d2H_GWout  <- NA
  }

  # Fill gaps in timeseries, values, and add notes on which gw well used
  monthly_isotopes <- fill_timeseries_gaps(monthly_isotopes, timeseries)
  monthly_isotopes <- iso_gapfill(monthly_isotopes)
  monthly_isotopes <- iso_gw_site_names(monthly_isotopes, isotopes)
  monthly_isotopes <- monthly_isotopes %>%
                      filter(.data$date %in% timeseries$dates)
  monthly_isotopes <- iso_lake_change(monthly_isotopes)

  # No lake or evap data during ice on
  ice <- cslsdata::ice
  ice_interval <- interval(round_date(ice$ice_on, unit = "month"),
                           round_date(ice$ice_off, unit = "month"))
  for (i in 1:nrow(monthly_isotopes)) {
    if (monthly_isotopes$date[i] %within% ice_interval) {
      monthly_isotopes$d18O_lake[i] <- NA
      monthly_isotopes$d2H_lake[i]  <- NA
    }
  }

  return(monthly_isotopes)
}
