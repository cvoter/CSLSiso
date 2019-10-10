#' Summarize Inputs over Common Timeseries
#'
#' This function takes raw weather, lake surface temperature, isotope, lake
#' level, groundwater level, stage-volume inputs and summarises water balance
#' inputs over a common timeseries.
#'
#' @param weather a data frame with sub-monthly weather including date, atmp (air
#'                temperature, deg C), RH (relative humidity, percent), P
#'                (precipitation, mm), Rs (incoming solar radiation, MJ/m^2),
#'                and wind (wind speed, m/s) as formatted in the
#'                \code{\link{weather}} dataset
#' @param lst a data frame with sub-monthly lake surface temperature
#'            measurements as formatted in the \code{\link{lst}} dataset, subset
#'            for a single lake.
#' @param isotopes a data frame with isotopes measurements as formatted in the
#'                 \code{\link{isotopes}} dataset, subset for a single lake.
#' @param lake_levels a data frame with daily water level measurements as
#'                    formatted in the \code{\link{water_levels}} dataset,
#'                    subset to lake level records for the lake of interest.
#' @param gw_levels a data frame with daily water level measurements as
#'                  formatted in the \code{\link{water_levels}} dataset,
#'                  subset to groundwater level records at the lake of
#'                  interest.
#' @param stage_vol a data frame with the lake, stage_m, surf_area_m2, and
#'                  volume_m3 as in the \code{\link{stage_vol}} dataset, subset
#'                  for a single lake.
#' @param site_dictionary a data frame with site id numbers and static isotope
#'                        site classifications as formatted in the
#'                        \code{\link{site_dictionary}} dataset, subset to
#'                        records for the lake of interest.
#' @param static_gw logical defaults to FALSE to use lake_levels and gw_levels
#'                  to define upgradient/downgradient wells at each measurement
#'                  date. If TRUE, uses static definitions of
#'                  upgradient/downgradient wells in site dictionary.
#' @param median_threshold minimum median difference between lake levels and
#'                         groundwater levels during the month of measurement in
#'                         order to classify groundwater measurement.
#' @param static_lake logical defaults to FALSE to use actual measurement for
#'                    each month. If TRUE, uses mean of fall (Sept-Nov) isotope
#'                    samples for the lake.
#' @param use_kniffin_pcpn logical defaults to TRUE to average in precipitation
#'                         measurements by Maribeth Kniffin for each month.
#'
#' @return h2o_bal_inputs, a data frame ordered by date with data only for the
#'         overlap timeseries for:
#' \describe{date}{dates of monthly timeseries (1st of the month) for months with
#'                 complete overlapping input data}
#' \describe{atmp_degC}{mean air temperature (degrees C)}
#' \describe{RH_pct}{mean relative humidity (percent)}
#' \describe{P_mm}{total precipitation (mm)}
#' \describe{ET_mm}{total lake evaporation (mm)}
#' \describe{atmp_K}{mean air temperature (Kelvin)}
#' \describe{ltmp_K}{mean lake surface temperature (degrees C)}
#' \describe{dV}{change in lake volume (mm)}
#' \describe{d18O_lake}{mean stable isotope measurement for lake}
#' \describe{d18O_pcpn}{mean stable isotope measurement for precipitation}
#' \describe{d18O_GWin}{mean stable isotope measurement for inflowing
#'                      groundwater}
#' \describe{d18O_GWout}{mean stable isotope measurement for outflowing
#'                       groundwater}
#' \describe{d18O_evap}{mean stable isotope measurement for evaporation}
#' \describe{GWin_sites}{wells included in mean inflowing groundwater stable
#'                       isotope measurement}
#' \describe{GWout_sites}{wells included in mean outflowing groundwater stable
#'                       isotope measurement}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @import lubridate
#'
#' @export

summarise_inputs <- function(weather, lst, isotopes, lake_levels, gw_levels,
                             stage_vol, site_dictionary, static_gw = FALSE,
                             median_threshold = 0.01, static_lake = FALSE,
                             use_kniffin_pcpn = TRUE){

  # Identify monthly timeseries with complete coverage of input data
  timeseries <- find_overlap_timeseries(weather, lst, isotopes, lake_levels,
                                        gw_levels)
  timeseries <- as_datetime(timeseries)

  # Summarize inputs over common timeseries
  monthly_weather   <- summarise_weather(weather, lst, stage_vol,
                                         site_dictionary, timeseries)
  monthly_lst       <- summarise_lst(lst, timeseries)
  monthly_dV        <- summarise_dV(lake_levels, timeseries)
  monthly_isotopes  <- summarise_isotopes(isotopes, site_dictionary, timeseries,
                                          static_gw, lake_levels,
                                          gw_levels, median_threshold,
                                          static_lake, use_kniffin_pcpn)
  monthly_isotopes  <- summarise_d18O_evap(monthly_weather, monthly_lst,
                                           monthly_isotopes)
  h2o_bal_inputs <- merge(monthly_weather, monthly_lst)
  h2o_bal_inputs <- merge(h2o_bal_inputs, monthly_dV)
  h2o_bal_inputs <- merge(h2o_bal_inputs, monthly_isotopes)

  h2o_bal_inputs <- h2o_bal_inputs %>%
                    select(.data$date, .data$atmp_degC, .data$RH_pct,
                           .data$P_mm, .data$ET_mm, .data$atmp_K, .data$ltmp_K,
                           .data$dV, .data$d18O_lake, .data$d18O_pcpn,
                           .data$d18O_GWin, .data$d18O_GWout, .data$d18O_evap,
                           .data$GWin_sites, .data$GWout_sites)

  return(h2o_bal_inputs)
}
