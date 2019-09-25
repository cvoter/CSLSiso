#' Monthly Evaporation d18O
#'
#' Calculates monthly Evaporation d18O based on monthly weather, lake surface
#' temperature, and isotope measurements.
#'
#' @param monthly_weather a data frame with monthly weather including date,
#'                        atmp_K (air temperature, K), RH_pct (relative humidity,
#'                        percent), P_mm (precipitation, mm), and ET_mm
#'                        (evapotranspiration, mm) as output by
#'                        \code{\link{get_monthly_weather}}
#' @param monthly_lst a data frame as output by \code{\link{get_monthly_lst}}
#' @param monthly_isotopes a data frame as output by
#'                         \code{\link{get_monthly_isotopes}}
#'
#' @return monthly_isotopes with d18O_evap column added
#'
#' @export

get_monthly_d18O_evap <- function(monthly_weather,
                                  monthly_lst,
                                  monthly_isotopes) {
  for (i in 1:nrow(monthly_isotopes)) {
    this_month <- monthly_isotopes$date[i]

    atmp       <- monthly_weather$atmp_K[monthly_weather$date == this_month]
    ltmp       <- monthly_lst$ltmp_K[monthly_lst$date == this_month]
    RH         <- monthly_weather$RH_pct[monthly_weather$date == this_month]
    d18O_pcpn  <- monthly_isotopes$d18O_pcpn[i]
    d18O_lake  <- monthly_isotopes$d18O_lake[i]

    if (any(is.na(c(atmp, ltmp, RH, d18O_pcpn, d18O_lake))) == FALSE &
        length(atmp) > 0 & length(ltmp) > 0 & length(RH) > 0 &
        length(d18O_pcpn) > 0 & length(d18O_lake) > 0 ) {
      monthly_isotopes$d18O_evap[i] <- d18O_evap(atmp, ltmp, RH,
                                                 d18O_pcpn, d18O_lake)
    } else {
      monthly_isotopes$d18O_evap[i] <- NA
    }
  }
  return(as.data.frame(monthly_isotopes))
}
