#' Monthly Evaporation d18O
#'
#' This function takes
#'
#' @param monthly_weather data frame with the following columns:
#' \itemize{
#' \item date - first of the month for each monthly observation (Date)
#' \item atmp_K - mean monthly air temperature (K)
#' \item relh_pct - mean monthly relative humidity (percent)
#' \item pcpn_mm - monthly precipitaiton (mm)
#' \item rpet_mm - monthly reference potential evapotranspiration (mm)
#' }
#' @param monthly_lst a data frame with the following columns:
#' \itemize{
#' \item date - first of the month for each monthly observation (Date)
#' \item ltmp_K - mean monthly lake surface temperature (degrees K)
#' }
#' @param monthly_isotopes a data frame with the following columns:
#' \itemize{
#' \item date - first of the month for each monthly observation (Date)
#' \item d18O_pcpn - mean precipitation stable isotope measurement for the month
#' \item d18O_lake - mean lake stable isotope measurement for the month
#' \item d18O_GWin - mean groundwater inflow stable isotope measurement for the
#'                  month
#' }
#'
#' @return monthly_isotopes with d18O_evap column added
#'
#' @export

get_monthly_d18O_evap <- function(monthly_weather,
                                  monthly_lst,
                                  monthly_isotopes){
  start_month <- min(monthly_isotopes$date)
  end_month   <- max(monthly_isotopes$date)
  month_info  <- start_n_months(c(start_month, end_month), all_days = FALSE)

  for (i in 1:month_info$nmonths) {
    this_month <- monthly_isotopes$date[i]

    atmp       <- monthly_weather$atmp_K[which(monthly_weather$date ==
                                                this_month)]
    ltmp       <- monthly_lst$ltmp_K[which(monthly_lst$date ==
                                             this_month)]
    RH         <- monthly_weather$relh_pct[which(monthly_weather$date ==
                                                  this_month)]
    d18O_pcpn  <- monthly_isotopes$d18O_pcpn[i]
    d18O_lake  <- monthly_isotopes$d18O_lake[i]

    monthly_isotopes$d18O_evap[i] <- d18O_evap(atmp,
                                               ltmp,
                                               RH,
                                               d18O_pcpn,
                                               d18O_lake)
  }
  return(as.data.frame(monthly_isotopes))
}
