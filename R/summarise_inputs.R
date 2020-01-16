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

summarise_inputs <- function(lake, weather, lst, isotopes, lake_levels,
                             gw_levels, dictionary, threshold = 0.01,
                             by_gw_iso = FALSE, annual = FALSE){

  # Identify monthly timeseries with complete coverage of input data
  timeseries <- find_timeseries(isotopes)

  # Summarize inputs over common timeseries
  monthly_weather   <- summarise_weather(weather, timeseries, lake)
  monthly_lst       <- summarise_lst(lst, timeseries)
  monthly_dV        <- summarise_dV(lake_levels, timeseries)
  monthly_isotopes  <- summarise_isotopes(isotopes, dictionary, timeseries,
                                          lake_levels, gw_levels, threshold)
  monthly_isotopes  <- summarise_d18O_evap(monthly_weather, monthly_lst,
                                           monthly_isotopes)
  inputs <- merge(monthly_weather, monthly_lst)
  inputs <- merge(inputs, monthly_dV)
  inputs <- merge(inputs, monthly_isotopes)

  inputs$P_m3 <- inputs$P_mm*inputs$mean_area_m2/1000
  inputs$E_m3 <- inputs$E_mm*inputs$mean_area_m2/1000

  inputs <- inputs %>%
            select(.data$date, .data$ltmp_degC, .data$ltmp_K, .data$atmp_degC,
                   .data$atmp_K, .data$RH_pct, .data$P_mm, .data$E_mm,
                   .data$dV_mm, .data$P_m3, .data$E_m3, .data$dV_m3,
                   .data$mean_vol_m3, .data$mean_area_m2, .data$d18O_lake,
                   .data$d18O_pcpn, .data$d18O_GWin, .data$d18O_GWout,
                   .data$d18O_evap, .data$d2H_lake, .data$d2H_pcpn,
                   .data$d2H_GWin, .data$d2H_GWout, .data$d2H_evap,
                   .data$delta_d18O_lake, .data$delta_d2H_lake,
                   .data$GWin_sites, .data$GWout_sites)
  if (annual){
    inputs <- inputs %>%
              mutate(d18O_evap = .data$d18O_evap*.data$E_m3/
                                 sum(.data$E_m3[!is.na(.data$d18O_evap)]),
                     d2H_evap = .data$d2H_evap*.data$E_m3/
                                sum(.data$E_m3[!is.na(.data$d2H_evap)]),
                     d18O_pcpn = .data$d18O_pcpn*.data$P_m3/
                                 sum(.data$P_m3[!is.na(.data$d18O_pcpn)]),
                     d2H_pcpn = .data$d2H_pcpn*.data$P_m3/
                                sum(.data$P_m3[!is.na(.data$d2H_pcpn)]),
                     delta_d18O_lake = .data$d18O_lake[.data$date == max(.data$date)] -
                                       .data$d18O_lake[.data$date == min(.data$date)],
                     delta_d2H_lake = .data$d2H_lake[.data$date == max(.data$date)] -
                                      .data$d2H_lake[.data$date == min(.data$date)]) %>%
              summarise(date = interval(min(.data$date), max(.data$date)),
                        P_mm = sum(.data$P_mm, na.rm = TRUE),
                        E_mm = sum(.data$E_mm, na.rm = TRUE),
                        dV_mm = sum(.data$dV_mm, na.rm = TRUE),
                        P_m3 = sum(.data$P_m3, na.rm = TRUE),
                        E_m3 = sum(.data$E_m3, na.rm = TRUE),
                        dV_m3 = sum(.data$dV_m3, na.rm = TRUE),
                        mean_vol_m3 = mean(.data$mean_vol_m3, na.rm = TRUE),
                        mean_area_m2 = mean(.data$mean_area_m2, na.rm = TRUE),
                        d18O_lake = mean(.data$d18O_lake, na.rm = TRUE),
                        d2H_lake = mean(.data$d2H_lake, na.rm = TRUE),
                        d18O_GWin = mean(.data$d18O_GWin, na.rm = TRUE),
                        d2H_GWin = mean(.data$d2H_GWin, na.rm = TRUE),
                        d18O_evap = sum(.data$d18O_evap, na.rm = TRUE),
                        d2H_evap = sum(.data$d2H_evap, na.rm = TRUE),
                        d18O_pcpn = sum(.data$d18O_pcpn, na.rm = TRUE),
                        d2H_pcpn = sum(.data$d2H_pcpn, na.rm = TRUE),
                        delta_d18O_lake = mean(.data$delta_d18O_lake),
                        delta_d2H_lake = mean(.data$delta_d2H_lake))
  }
  return(inputs)
}
