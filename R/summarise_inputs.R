#' Summarize Inputs over Common Timeseries
#'
#' This function takes raw weather, lake surface temperature, isotope, lake
#' level, groundwater level, stage-volume inputs and summarises water balance
#' inputs over a common timeseries.
#'
#' @inheritParams summarise_h2o_bal
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
                             elev_area_vol, dictionary, static_gw = FALSE,
                             threshold = 0.01, static_lake = FALSE,
                             use_kniffin_pcpn = TRUE, extend_pcpn = TRUE,
                             by_gw_iso = TRUE){

  # Identify monthly timeseries with complete coverage of input data
  analysis <- find_timeseries(weather, lst, isotopes, lake_levels, gw_levels,
                              dictionary, static_gw, threshold, by_gw_iso)

  # Summarize inputs over common timeseries
  monthly_weather   <- summarise_weather(weather, lst, elev_area_vol,
                                         dictionary, analysis)
  monthly_lst       <- summarise_lst(lst, analysis)
  monthly_dV        <- summarise_dV(lake_levels, analysis)
  monthly_isotopes  <- summarise_isotopes(isotopes, dictionary, analysis,
                                          static_gw, lake_levels,
                                          gw_levels, threshold,
                                          static_lake, use_kniffin_pcpn,
                                          extend_pcpn)
  monthly_isotopes  <- summarise_d18O_evap(monthly_weather, monthly_lst,
                                           monthly_isotopes)
  h2o_bal_inputs <- merge(monthly_weather, monthly_lst)
  h2o_bal_inputs <- merge(h2o_bal_inputs, monthly_dV)
  h2o_bal_inputs <- merge(h2o_bal_inputs, monthly_isotopes)

  h2o_bal_inputs$P_m3 <- h2o_bal_inputs$P_mm*h2o_bal_inputs$mean_area_m2/1000
  h2o_bal_inputs$E_m3 <- h2o_bal_inputs$E_mm*h2o_bal_inputs$mean_area_m2/1000

  h2o_bal_inputs <- h2o_bal_inputs %>%
                    select(.data$date, .data$ltmp_degC, .data$ltmp_K,
                           .data$atmp_degC, .data$atmp_K, .data$RH_pct,
                           .data$P_mm, .data$E_mm, .data$dV_mm, .data$P_m3,
                           .data$E_m3, .data$dV_m3, .data$mean_vol_m3,
                           .data$mean_area_m2, .data$d18O_lake,
                           .data$d18O_pcpn, .data$d18O_GWin, .data$d18O_GWout,
                           .data$d18O_evap, .data$d2H_lake, .data$d2H_pcpn,
                           .data$d2H_GWin, .data$d2H_GWout, .data$d2H_evap,
                           .data$delta_d18O_lake, .data$delta_d2H_lake,
                           .data$GWin_sites, .data$GWout_sites)
  return(h2o_bal_inputs)
}
