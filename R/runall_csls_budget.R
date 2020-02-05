#' Run all functions needed to calculate water budget for a CSLS lake
#'
#' Runs all major functions in the CSLSiso package, from loading in raw data to
#' the final water balance for one of the CSLS lakes. Can be calculated on a
#' monthly basis (annual = FALSE) or based on all available data (annual =
#' TRUE).
#'
#' Loaded data includes the following data in a list format, where each lake has
#' its own data frame. For example, to access groundwater levels for Pleasant
#' Lake, the command would be: \code{gw_levels <- CSLSdata::gw_levels[["Pleasant"]]}.
#' \itemize{
#' \item \code{\link[CSLSdata]{weather}}, hourly weather data including "date",
#'       "atmp" (air temperature, deg C), "P" (precipitation, mm), "RH"
#'       (relative humidity, percent), "Rs" (solar radiation, MJ), and "wind"
#'       (wind speed, m/s)
#' \item \code{\link[CSLSdata]{lst_HOBO}}, hourly lake surface temperature from
#'       HOBO loggers, including "date", "ltmp" (lake surface temperature, deg
#'       C), and "elev_m" (elevation of the sensor, meters above mean sea level)
#' \item \code{\link[CSLSdata]{isotopes}}, stable isotope measurements including
#'       "date", "lake", "site_id", "d18O" measurement, and "d2H" measurement
#' \item \code{\link[CSLSdata]{lake_levels}}, daily lake levels (meters above
#'       mean sea level) including "date", "
#' \item \code{\link[CSLSdata]{gw_levels}}, daily groundwater levels including
#'       date, site_no, obs_type ("GW"), level_m (level in meters above mean sea
#'       level), and site_id (CSLS site id)
#' \item \code{\link[CSLSdata]{dictionary}}, links site ids and characteristics
#'       together with columns for lake, obs_type ("LK", "GW", "P"), site_id
#'       (e.g., "LL-01"), SWIMS_station_id, USGS_id, WBIC, static_iso_class
#'       (e.g., "upgradient", "lake", "precipitation"), lat_deg, long_deg,
#'       elev_m, and bouy_bottom_elev_m
#' }
#'
#' @inheritParams summarise_h2o_bal
#'
#' @return h2o_bal, a data frame with the following columns:
#' \item{date}{observation dates, last day of the month (if monthly) or date
#'             interval (if annual) (POSIXct)}
#' \item{P_m3}{precipitation for month or year (m3)}
#' \item{E_m3}{evapotranspiration for month or year (m3)}
#' \item{GWin_m3}{groundwater inflow to the lake for month or year (m3)}
#' \item{GWout_m3}{groundwater outflow to the lake for month or year (m3)}
#' \item{dV_m3}{change in lake volume for month or year (m3)}
#' \item{P_mm}{precipitation for month or year (mm)}
#' \item{E_mm}{evapotranspiration for month or year (mm)}
#' \item{GWin_mm}{groundwater inflow to the lake for month or year (mm)}
#' \item{GWout_mm}{groundwater outflow to the lake for month or year (mm)}
#' \item{dV_mm}{change in lake volume for month or year (mm)}
#' \item{mean_vol_m3}{mean lake volume during the month or year (m3)}
#' \item{mean_area_m2}{mean lake area during the month or year (m2)}
#'
#' @import CSLSdata
#'
#' @export

runall_csls_budget <- function(lake, threshold = 0.01, start_date = NULL,
                               end_date = NULL, annual = FALSE){
    weather         <- CSLSdata::weather
    lst             <- CSLSdata::lst_HOBO[[lake]]
    isotopes        <- CSLSdata::isotopes[[lake]]
    lake_levels     <- CSLSdata::lake_levels[[lake]]
    gw_levels       <- CSLSdata::gw_levels[[lake]]
    dictionary      <- CSLSdata::dictionary[[lake]]
    h2o_bal         <- summarise_h2o_bal(lake, weather, lst, isotopes,
                                         lake_levels, gw_levels, dictionary,
                                         threshold, start_date, end_date,
                                         annual)
  return(h2o_bal)
}
