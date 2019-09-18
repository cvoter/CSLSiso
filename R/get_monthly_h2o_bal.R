#' Monthly Water Balance
#'
#' Calculates the water balance of a lake based measured fluxes and isotopic
#' signatures of precipitation, the lake, and inflowing groundwater wells. Based
#' on equations 2 and 4 in Krabbenhoft et al. (1990).
#'
#' @references Krabbenhoft, D. P., C. J. Bowser, M. P. Anderson, and J. W.
#'   Valley. (1990). Estimating Groundwater Exchange with Lakes: 1. The Stable
#'   Isotope Mass Balance Method. Water Resources Research, 26(10):2445-2453.
#'   https://doi.org/10.1029/WR026i010p02445
#'
#' @param monthly_weather data frame with the following columns:
#' \itemize{
#' \item date - first of the month for each monthly observation (Date)
#' \item atmp_K - mean monthly air temperature (K)
#' \item relh_pct - mean monthly relative humidity (percent)
#' \item pcpn_mm - monthly precipitaiton (mm)
#' \item rpet_mm - monthly reference potential evapotranspiration (mm)
#' }
#' @param monthly_isotopes a data frame with the following columns:
#' \itemize{
#' \item date - first of the month for each monthly observation (Date)
#' \item d18O_pcpn - mean precipitation stable isotope measurement for the month
#' \item d18O_lake - mean lake stable isotope measurement for the month
#' \item d18O_GWin - mean groundwater inflow stable isotope measurement for the
#'                  month
#' \item d18O_evap - evaporation stable isotope composition for the month
#' }
#'
#' @return lake_h2o_bal - a data frame with the following columns;
#' \describe{
#' \item{date}{first of the month for each monthly observation (Date)}
#' \item{P}{monthly precipitation (mm)}
#' \item{E}{monthly evapotranspiration (mm)}
#' \item{GWin}{monthly groundwater inflow to the lake (mm)}
#' \item{GWout}{monthly groundwater outflow to the lake (mm)}
#' }
#'
#' @importFrom lubridate as_datetime
#'
#' @export

get_monthly_h2o_bal <- function(monthly_weather, monthly_isotopes) {
  start_month <- min(monthly_isotopes$date)
  end_month   <- max(monthly_isotopes$date)
  month_info  <- start_n_months(c(start_month, end_month), all_days = FALSE)

  lake_h2o_bal <- NULL
  for (i in 1:month_info$nmonths) {
    this_month <- monthly_isotopes$date[i]

    # Required inputs
    P          <- monthly_weather$pcpn_mm[which(monthly_weather$date ==
                                                  this_month)]
    E          <- monthly_weather$rpet_mm[which(monthly_weather$date ==
                                                  this_month)]
    d18O_pcpn  <- monthly_isotopes$d18O_pcpn[i]
    d18O_lake  <- monthly_isotopes$d18O_lake[i]
    d18O_GWin  <- monthly_isotopes$d18O_GWin[i]
    d18O_evap  <- monthly_isotopes$d18O_evap[i]

    # Monthly lake water balance
    lake_h2o_bal$date[i] <- this_month
    lake_h2o_bal$P[i]    <- P
    lake_h2o_bal$E[i]    <- E
    lake_h2o_bal$GWin[i] <- GW_inflow(P,
                                      E,
                                      d18O_pcpn,
                                      d18O_lake,
                                      d18O_GWin,
                                      d18O_evap)
    lake_h2o_bal$GWout[i] <- GW_outflow(P, E, lake_h2o_bal$GWin[i], dVdt = 0)
  }
  # R bizzarly looses the class of date objects in for loops, fix here
  # Format of date number requires lubridate::as_datetime, but convert from
  # POSIXct to Date for ggplotting
  lake_h2o_bal$date <- as_datetime(lake_h2o_bal$date)
  lake_h2o_bal$date <- as.Date(lake_h2o_bal$date)

  return(as.data.frame(lake_h2o_bal))
}
