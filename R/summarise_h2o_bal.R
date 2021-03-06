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
#' @param lake name of lake (e.g., Pleasant, Long, or Plainfield)
#' @param weather a data frame with sub-monthly weather including including date
#'       (POSIXct), atmp (air temperature, deg C), P (precipitation, mm), RH
#'       (relative humidity, percent), Rs (solar radiation, MJ), and wind (wind
#'       speed, m/s)
#' @param lst a data frame with sub-monthly lake surface temperature
#'            measurements as formatted in the lst_HOBO dataset, subset
#'            for a single lake.
#' @param isotopes a data frame with isotopes measurements as formatted in the
#'                 isotopes dataset, subset for a single lake.
#' @param lake_levels a data frame with daily water level measurements as
#'                    formatted in the lake_levels dataset,
#'                    subset to lake level records for the lake of interest.
#' @param gw_levels a data frame with daily water level measurements as
#'                  formatted in the lake_levels dataset,
#'                  subset to groundwater level records at the lake of
#'                  interest.
#' @param dictionary a data frame with site id numbers and static isotope
#'                        site classifications as formatted in the
#'                        dictionary dataset, subset to
#'                        records for the lake of interest.
#' @param threshold minimum median difference between lake levels and
#'                  groundwater levels during the month of measurement in order
#'                  to classify groundwater measurement.
#' @param start_date optional start date to use for analysis (first day of the
#'                   month) in POSIXct. If not provided, defaults to NULL and
#'                  timeseries is based on months with isotope samples.
#' @param end_date optional end date to use for analysis (last day of the
#'                   month) in POSIXct. If not provided, defaults to NULL and
#'                  timeseries is based on months with isotope samples.
#' @param annual defaults to FALSE to calculate water balance on a monthly
#'               basis. If TRUE, calculates the annual balance instead.
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
#' @importFrom NISTunits NISTsecTOyear NISTdayTOsec
#'
#' @export

summarise_h2o_bal <- function(lake, weather, lst, isotopes, lake_levels,
                              gw_levels, dictionary, threshold = 0.01,
                              start_date = NULL, end_date = NULL, annual = FALSE) {

  # Summarise inputs over same monthly timeseries
  inputs  <- summarise_inputs(lake, weather, lst, isotopes, lake_levels,
                              gw_levels, dictionary, threshold, start_date,
                              end_date, annual)

  # Calculate remaining water balance terms
  h2o_bal <- calculate_h2o_bal(inputs)

  if (annual) {
    yrs <- NISTsecTOyear(int_length(h2o_bal$date) + NISTdayTOsec(30))
    h2o_bal$res_time <- h2o_bal$mean_vol_m3/
                       ((h2o_bal$P_m3 + h2o_bal$GWin_m3)/yrs)
  }

  return(h2o_bal)
}
