#' Run functions to summarise and gather inputs for a CSLS lake
#'
#' Runs all major functions in the isoH2Obudget package to transform raw data
#' into the final inputs data frame for one of the CSLS lakes.
#'
#' Loaded data includes the following data in a list format, where each lake has
#' its own data frame. For example, to access groundwater levels for Pleasant
#' Lake, the command would be \code{cslsdata::gw_levels[["Pleasant"]]}.
#' \itemize{
#' \item \code{\link[cslsdata]{weather}}, hourly weather data including date
#'       (POSIXct), atmp (air temperature, deg C), P (precipitation, mm), RH
#'       (relative humidity, percent), Rs (solar radiation, MJ), and wind (wind
#'       speed, m/s)
#' \item \code{\link[cslsdata]{lst_HOBO}}, hourly lake surface temperature from
#'       HOBO loggers, including date (POSIXct), ltmp (lake surface temperature,
#'       deg C), and elev_m (elevation of the sensor, meters above mean sea
#'       level)
#' \item \code{\link[cslsdata]{isotopes}}, stable isotope measurements including
#'       date (POSIXct), lake, site_id, d18O measurement, and d2H measurement
#' \item \code{\link[cslsdata]{lake_levels}}, daily lake levels (meters above
#'       mean sea level) including date (POSIXct), site_no (USGS), obs_type
#'       ("LK"), and level_m (level in meters above mean sea level)
#' \item \code{\link[cslsdata]{gw_levels}}, daily groundwater levels including
#'       date, site_no, obs_type ("GW"), level_m (level in meters above mean sea
#'       level), and site_id (CSLS site id)
#' \item \code{\link[cslsdata]{elev_area_vol}}, storage capacity information,
#'       including elev_m (lake level elevation in meters above mean sea level),
#'       area_m2 (lake surface area, m^2), and vol_m3 (lake volume, m^3)
#' \item \code{\link[cslsdata]{dictionary}}, links site ids and characteristics
#'       together with columns for lake, obs_type ("LK", "GW", "P"), site_id
#'       (e.g., "LL-01"), SWIMS_station_id, USGS_id, WBIC, static_iso_class
#'       (e.g., "upgradient", "lake", "precipitation"), lat_deg, long_deg,
#'       elev_m, and bouy_bottom_elev_m
#' }
#'
#' @param lake name of lake (e.g., Pleasant, Long, or Plainfield)
#'
#' @return monthly_inputs, a data frame with the following columns:
#' \describe{
#' \item{date}{date of month}
#' \item{atmp_degC}{average air temperature, degrees C}
#' \item{RH_pct}{average relative humidity, percent}
#' \item{P_mm}{total precipitaiton, mm}
#' \item{ET_mm}{total lake evaporation, mm}
#' \item{atmp_K}{average air temperature, Kelvin}
#' \item{ltmp_K}{average lake surface tempearture, Kelvin}
#' \item{dV}{change in lake volume, mm}
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
#' }
#'
#' @import cslsdata
#'
#' @export

runall_csls_inputs <- function(lake){
    weather         <- cslsdata::weather
    lst             <- cslsdata::lst_HOBO[[lake]]
    isotopes        <- cslsdata::isotopes[[lake]]
    lake_levels     <- cslsdata::lake_levels[[lake]]
    gw_levels       <- cslsdata::gw_levels[[lake]]
    elev_area_vol   <- cslsdata::elev_area_vol[[lake]]
    dictionary      <- cslsdata::dictionary[[lake]]
    monthly_inputs  <- summarise_inputs(weather, lst, isotopes, lake_levels,
                                        gw_levels, elev_area_vol, dictionary)
  return(monthly_inputs)
}