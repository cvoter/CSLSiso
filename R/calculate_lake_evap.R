#' Calculate Lake Evaporation
#'
#' Calculates Lake Evaporation
#'
#' @param weather a data frame with sub-monthly weather including date, atmp (air
#'                temperature, deg C), RH (relative humidity, percent), P
#'                (precipitation, mm), Rs (incoming solar radiation, MJ/m^2),
#'                and wind (wind speed, m/s) as formatted in the
#'                \code{\link{weather}} dataset.
#' @param lst a data frame with sub-monthly lake surface temperature
#'            measurements as formatted in the \code{\link{lst}} dataset, subset
#'            for a single lake.
#' @param stage_vol a data frame with the lake, stage_m, surf_area_m2, and
#'                  volume_m3 as in the \code{\link{stage_vol}} dataset, subset
#'                  for a single lake.
#' @param site_dictionary a data frame with the lake, obs_type, site_id,
#'                        SWIMS_station, USGS_id, WBIC, and static_iso_type of
#'                        measurement sites, as in the
#'                        \code{\link{site_dictionary}} dataset, subset for a
#'                        single lake.
#' @param Lz the longitude of the local timezone (degrees west of Greenwich,
#'           ranges from 0 to 360 degrees). Defaults to 90 for Central Time
#'           Zone, USA.
#' @param lake_albedo albedo of the lake, defaults to 0.08 for open water.
#' @param wind_elev height of wind measurement, defaults to 3m for Hancock
#'                  station.
#' @param z0 aeorodynamic roughness of landcover at site of weather measurements
#'          (m), defaults to 0.02 for a grass.
#'
#'
#' @return weather, a list with daily weather information including:
#' \describe{
#' \item{date}{day of each weather observation}
#' \item{atmp_min}{minimum air temperature for the day (deg C)}
#' \item{atmp_max}{maximum air temperature for the day (deg C)}
#' \item{RH_min}{minimum relative humidity for the day (percent)}
#' \item{RH_max}{maximum relative humidity for the day (percent)}
#' \item{P}{total precipitation for the day (mm)}
#' \item{ET}{total lake evaporation for the day (mm)}
#' }
#'
#' @importFrom faoET McJannet_lake_ET
#'
#' @export
calculate_lake_evap <- function(weather, lst, stage_vol, site_dictionary,
                                Lz = 90, lake_albedo = 0.08, wind_elev = 3,
                                z0 = 0.02) {
  # Location info for lake evaporation
  loc <- format_lake_loc(site_dictionary, Lz)

  # Lake info for lake evaporation
  lake <- format_lake_info(stage_vol)

  # Determine first day with both lst and weather data
  lst_day_one     <- floor_date(lst$date[which.min(lst$date)], unit = "day")
  weather_day_one <- floor_date(weather$date[which.min(weather$date)], unit = "day")
  day_one         <- max(c(lst_day_one, weather_day_one))

  # Weather info for lake evaporation
  weather       <- format_lake_weather(weather, day_one, wind_elev, z0)
  weather$wtmp0 <- lst$ltmp[which(floor_date(lst$date, unit = "day") == day_one)]

  # Lake water albedo for lake evaporation
  albedo <- list(lake = lake_albedo)

  # Lake evaporation
  ET <- McJannet_lake_ET(loc, lake, weather, albedo)

  # Combine daily weather values into new weather dataframe
  weather <- as.data.frame(cbind(weather$datetimes, weather$atmp$min,
                                 weather$atmp$max, weather$RH$min,
                                 weather$RH$max, weather$P, ET))
  colnames(weather) <- c("date", "atmp_min", "atmp_max", "RH_min", "RH_max",
                         "P", "ET")
  weather$date <- as_datetime(weather$date)

  return(weather)
}
