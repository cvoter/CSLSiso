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
#' @param monthly_weather a data frame as output by
#'                        \code{\link{get_monthly_weather}}
#' @param monthly_isotopes a data frame as output by
#'                         \code{\link{get_monthly_isotopes}}
#' @param monthly_dV a data frame as output by
#'                         \code{\link{get_monthly_dV}}
#' @param month_info a list with with the start date, end date, and number of
#'                   months in the common timeseries of input data, as output by
#'                   \code{\link{get_overlap_months}}
#'
#' @return monthly_h2o_bal, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation (Date)}
#' \item{P}{monthly precipitation (mm)}
#' \item{E}{monthly evapotranspiration (mm)}
#' \item{GWin}{monthly groundwater inflow to the lake (mm)}
#' \item{GWout}{monthly groundwater outflow to the lake (mm)}
#' \item{dV}{monthly change in lake volume (mm)}
#' }
#'
#' @import lubridate
#'
#' @export

get_monthly_h2o_bal <- function(monthly_weather,
                                monthly_isotopes,
                                monthly_dV = NULL,
                                month_info) {
  monthly_h2o_bal <- NULL
  for (i in 1:month_info$nmonths) {
    this_month <- month_info$start_date + months(i - 1)

    # Required inputs
    P          <- monthly_weather$P_mm[monthly_weather$date == this_month]
    E          <- monthly_weather$ET_mm[monthly_weather$date == this_month]
    if (is.null(monthly_dV)) {
      dV       <- 0
    } else {
      dV       <- monthly_dV$dV[monthly_dV$date == this_month]
    }
    d18O_pcpn  <- monthly_isotopes$d18O_pcpn[monthly_isotopes$date == this_month]
    d18O_lake  <- monthly_isotopes$d18O_lake[monthly_isotopes$date == this_month]
    d18O_GWin  <- monthly_isotopes$d18O_GWin[monthly_isotopes$date == this_month]
    d18O_evap  <- monthly_isotopes$d18O_evap[monthly_isotopes$date == this_month]

    # Monthly lake water balance
    monthly_h2o_bal$date[i] <- this_month
    monthly_h2o_bal$P[i]    <- P
    monthly_h2o_bal$E[i]    <- E
    monthly_h2o_bal$GWin[i] <- calculate_GW_inflow(P,
                                                   E,
                                                   d18O_pcpn,
                                                   d18O_lake,
                                                   d18O_GWin,
                                                   d18O_evap)
    monthly_h2o_bal$GWout[i] <- calculate_GW_outflow(P,
                                                     E,
                                                     monthly_h2o_bal$GWin[i],
                                                     dV)
    monthly_h2o_bal$dV[i]   <- dV
  }
  # R bizzarly looses the class of date objects in for loops, fix here
  # Format of date number requires lubridate::as_datetime, but convert from
  # POSIXct to Date for ggplotting
  monthly_h2o_bal$date <- as_datetime(monthly_h2o_bal$date)
  monthly_h2o_bal$date <- as.Date(monthly_h2o_bal$date)

  return(as.data.frame(monthly_h2o_bal))
}
