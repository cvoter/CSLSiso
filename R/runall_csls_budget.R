#' Run isoH2Obudget start to finish for a CSLS lake
#'
#' Runs all major functions in the isoH2Obudget package, from loading in raw
#' data to the final monthly water balance for one of the CSLS lakes.
#'
#' Loaded data includes the following data in a list format, where each lake has
#' its own data frame. For example, to access groundwater levels for Pleasant
#' Lake, the command would be \code{cslsdata::gw_levels[["Pleasant"]]}.
#' \itemize{
#' \item \code{\link[cslsdata]{weather}}, hourly weather data including date,
#'       atmp (air temperature, deg C), P (precipitation, mm), RH (relative
#'       humidity, percent), Rs (solar radiation, MJ), and wind (wind speed,
#'       m/s)
#' \item \code{\link[cslsdata]{lst_HOBO}}, hourly lake surface temperature from
#'       HOBO loggers, including date, ltmp (lake surface temperature, deg C),
#'       and elev_m (elevation of the sensor, meters above mean sea level)
#' \item \code{\link[cslsdata]{isotopes}}, stable isotope measurements including
#'       date, lake, site_id, d18O measurement, and d2H measurement
#' \item \code{\link[cslsdata]{lake_levels}}, daily lake levels (meters above
#'       mean sea level)
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
#' @inheritParams summarise_h2o_bal
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
#' @import cslsdata
#'
#' @export

runall_csls_budget <- function(lake, static_gw = FALSE, threshold = 0.01,
                               static_lake = FALSE, use_kniffin_pcpn = TRUE,
                               extend_pcpn = TRUE, by_gw_iso = TRUE){
    weather         <- cslsdata::weather
    lst             <- cslsdata::lst_HOBO[[lake]]
    isotopes        <- cslsdata::isotopes[[lake]]
    lake_levels     <- cslsdata::lake_levels[[lake]]
    gw_levels       <- cslsdata::gw_levels[[lake]]
    elev_area_vol   <- cslsdata::elev_area_vol[[lake]]
    dictionary      <- cslsdata::dictionary[[lake]]
    monthly_h2o_bal <- summarise_h2o_bal(weather, lst, isotopes,
                                         lake_levels, gw_levels, dictionary,
                                         elev_area_vol, static_gw,
                                         threshold, static_lake,
                                         use_kniffin_pcpn, extend_pcpn,
                                         by_gw_iso)
  return(monthly_h2o_bal)
}
