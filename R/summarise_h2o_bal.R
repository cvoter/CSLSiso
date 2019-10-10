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

calculate_h2o_bal <- function(lake, weather, lst, isotopes, water_levels,
                              site_dictionary, stage_vol, static_gw = FALSE,
                              median_threshold = 0.01, static_lake = FALSE,
                              use_kniffin_pcpn = TRUE) {
  # Subset for lake
  lst             <- subset_lst(lake, lst, site_dictionary)
  isotopes        <- subset_isotopes(lake, isotopes)
  lake_levels     <- subset_lake_levels(lake, water_levels, site_dictionary)
  gw_levels       <- subset_gw_levels(lake, water_levels, site_dictionary)
  stage_vol       <- subset_stage_vol(lake, stage_vol)
  site_dictionary <- subset_site_dictionary(lake, site_dictionary)

  # Summarise inputs over same monthly timeseries
  h2o_bal_inputs  <- summarise_inputs(weather, lst, isotopes, lake_levels,
                                      gw_levels, stage_vol, site_dictionary,
                                      static_gw, median_threshold, static_lake,
                                      use_kniffin_pcpn)

  # Calculate remaining water balance terms
  monthly_h2o_bal <- calculate_h2o_bal(h2o_bal_inputs)

  return(monthly_h2o_bal)
}
