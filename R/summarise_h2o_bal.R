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
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param weather a data frame with sub-monthly weather including date, atmp (air
#'                temperature, deg C), RH (relative humidity, percent), P
#'                (precipitation, mm), Rs (incoming solar radiation, MJ/m^2),
#'                and wind (wind speed, m/s) as formatted in the
#'                \code{\link{weather}} dataset
#' @param lst a data frame with sub-monthly lake surface temperature
#'            measurements as formatted in the \code{\link{lst}} dataset.
#' @param isotopes a data frame with isotopes measurements as formatted in the
#'                 \code{\link{isotopes}} dataset.
#' @param lake_levels a data frame with daily lake level measurements as
#'                    formatted in the \code{\link{water_levels}} dataset.
#' @param gw_levels a data frame with daily groundwater level measurements as
#'                    formatted in the \code{\link{water_levels}} dataset.
#' @param stage_vol a data frame with the lake, stage_m, surf_area_m2, and
#'                  volume_m3 as in the \code{\link{stage_vol}} dataset.
#' @param site_dictionary a data frame with site id numbers and static isotope
#'                        site classifications as formatted in the
#'                        \code{\link{site_dictionary}} dataset.
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
#' @param pcpnfile name of file with dates of precipitation collector deployment.
#'                 Defaults to "csls_isotope_precipitation_deployment.csv"
#' @param pcpndir name of directory with file with dates of precipitation
#'                collector deployment. Defaults to "system.file" to indicate is
#'                stored within inst/extdata within the installed isoH2Obudget
#'                package files.
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

summarise_h2o_bal <- function(lake, weather, lst, isotopes, lake_levels,
                              gw_levels, site_dictionary, stage_vol,
                              static_gw = FALSE, median_threshold = 0.01,
                              static_lake = FALSE, use_kniffin_pcpn = TRUE,
                              pcpnfile = 'csls_isotope_precipitation_deployment.csv',
                              pcpndir = "system.file") {
  # Summarise inputs over same monthly timeseries
  h2o_bal_inputs  <- summarise_inputs(weather, lst, isotopes, lake_levels,
                                      gw_levels, stage_vol, site_dictionary,
                                      static_gw, median_threshold, static_lake,
                                      use_kniffin_pcpn, pcpnfile, pcpndir)

  # Calculate remaining water balance terms
  monthly_h2o_bal <- calculate_h2o_bal(h2o_bal_inputs)

  return(monthly_h2o_bal)
}
