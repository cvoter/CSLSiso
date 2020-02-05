#' Run all functions needed to summarise water budget inputs for a CSLS lake
#'
#' Runs all major functions in the CSLSiso package to transform raw data into
#' the final inputs data frame for one of the CSLS lakes. Can be calculated on a
#' monthly basis (annual = FALSE) or based on all available data (annual =
#' TRUE).
#'
#' Loaded data includes the following data in a list format, where each lake has
#' its own data frame. For example, to access groundwater levels for Pleasant
#' Lake, the command would be \code{gw_levels <- CSLSdata::gw_levels[["Pleasant"]]}.
#' \itemize{
#' \item \code{\link[CSLSdata]{weather}}, hourly weather data including date
#'       (POSIXct), atmp (air temperature, deg C), P (precipitation, mm), RH
#'       (relative humidity, percent), Rs (solar radiation, MJ), and wind (wind
#'       speed, m/s)
#' \item \code{\link[CSLSdata]{lst_HOBO}}, hourly lake surface temperature from
#'       HOBO loggers, including date (POSIXct), ltmp (lake surface temperature,
#'       deg C), and elev_m (elevation of the sensor, meters above mean sea
#'       level)
#' \item \code{\link[CSLSdata]{isotopes}}, stable isotope measurements including
#'       date (POSIXct), lake, site_id, d18O measurement, and d2H measurement
#' \item \code{\link[CSLSdata]{lake_levels}}, daily lake levels (meters above
#'       mean sea level) including date (POSIXct), site_no (USGS), obs_type
#'       ("LK"), and level_m (level in meters above mean sea level)
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
#' @return monthly_inputs, a data frame with the following columns:
#' \item{date}{date of month}
#' \item{ltmp_degC}{average lake surface temperature, degrees C}
#' \item{ltmp_K}{average lake surface temperature, Kelvin}
#' \item{atmp_degC}{average air temperature, degrees C}
#' \item{atmp_K}{average air temperature, Kelvin}
#' \item{RH_pct}{average relative humidity, percent}
#' \item{P_mm}{precipitaiton for month or year, mm}
#' \item{E_mm}{lake evaporation for month or year, mm}
#' \item{dV_mm}{change in lake volume for month or year, mm}
#' \item{P_m3}{precipitation for month or year, m3}
#' \item{E_m3}{lake evaporation for month or year, m3}
#' \item{dV_m3}{change in lake volume for month or year, m3}
#' \item{mean_vol_m3}{mean lake volume during the month or year (m3)}
#' \item{mean_area_m2}{mean lake area during the month or year (m2)}
#' \item{d18O_lake}{mean stable isotope measurement for d18O, lake (per mil)}
#' \item{d18O_pcpn}{mean stable isotope measurement for d18O, precipitation
#'                  (per mil)}
#' \item{d18O_GWin}{mean stable isotope measurement for d18O, GWin (per mil)}
#' \item{d18O_GWout}{mean stable isotope measurement for d18O, GWout (per mil)}
#' \item{d18O_evap}{mean stable isotope measurement for d18O, evaporation (per
#'                  mil)}
#' \item{d2H_lake}{mean stable isotope measurement for d2H, lake (per mil)}
#' \item{d2H_pcpn}{mean stable isotope measurement for d2H, precipitation (per
#'                 mil)}
#' \item{d2H_GWin}{mean stable isotope measurement for d2H, GWin (per mil)}
#' \item{d2H_GWout}{mean stable isotope measurement for d2H, GWout (per mil)}
#' \item{d2H_evap}{mean stable isotope measurement for d2H, evaporation (per
#'                 mil)}
#' \item{GWin_sites}{name of monitoring wells use for GWin isotopes}
#' \item{GWout_sites}{name of monitoring wells use for GWout isotopes}
#'
#' @import CSLSdata
#'
#' @export

runall_csls_inputs <- function(lake, threshold = 0.01, start_date = NULL,
                               end_date = NULL, annual = FALSE){
    weather       <- CSLSdata::weather
    lst           <- CSLSdata::lst_HOBO[[lake]]
    isotopes      <- CSLSdata::isotopes[[lake]]
    lake_levels   <- CSLSdata::lake_levels[[lake]]
    gw_levels     <- CSLSdata::gw_levels[[lake]]
    dictionary    <- CSLSdata::dictionary[[lake]]
    inputs        <- summarise_inputs(lake, weather, lst, isotopes, lake_levels,
                                      gw_levels, dictionary, threshold,
                                      start_date, end_date, annual)
  return(inputs)
}
