#' Summarize Evaporation d18O
#'
#' Calculates Evaporation d18O based on monthly weather, lake surface
#' temperature, and isotope measurements.
#'
#' @param monthly_weather a data frame with monthly weather including date,
#'                        atmp_K (air temperature, K), RH_pct (relative humidity,
#'                        percent), P_mm (precipitation, mm), and ET_mm
#'                        (evapotranspiration, mm) as output by
#'                        \code{\link{summarise_weather}}
#' @param monthly_lst a data frame as output by \code{\link{summarise_lst}}
#' @param monthly_isotopes a data frame as output by
#'                         \code{\link{summarise_isotopes}}
#'
#' @return monthly_isotopes with d18O_evap and d2H_evap columns added
#'
#' @export

summarise_d18O_evap <- function(monthly_weather, monthly_lst,
                                monthly_isotopes) {
  del_evap <- merge(monthly_weather, monthly_lst)
  del_evap <- merge(del_evap, monthly_isotopes)
  del_evap <- del_evap %>%
              mutate(d18O_evap = calculate_d18O_evap(.data$atmp_K,
                                                     .data$ltmp_K,
                                                     .data$RH_pct,
                                                     .data$d18O_pcpn,
                                                     .data$d18O_lake),
                     d2H_evap = calculate_d18O_evap(.data$atmp_K,
                                                    .data$ltmp_K,
                                                    .data$RH_pct,
                                                    .data$d2H_pcpn,
                                                    .data$d2H_lake)) %>%
              select(.data$date, .data$d18O_evap, .data$d2H_evap)
  monthly_isotopes <- merge(monthly_isotopes, del_evap)

  return(monthly_isotopes)
}
