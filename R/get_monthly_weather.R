#' Monthly Weather
#'
#' Summarizes sub-monthly weather at a monthly timestep
#'
#' @param weather a data frame with sub-monthly weather including date, atmp (air
#'                temperature, deg C), RH (relative humidity, percent), P
#'                (precipitation, mm), Rs (incoming solar radiation, MJ/m^2),
#'                and wind (wind speed, m/s) as formatted in the
#'                \code{\link{weather}} dataset.
#' @param lst a data frame with sub-monthly lake surface temperature
#'            measurements as formatted in the \code{\link{lst}} dataset, subset
#'            for a single lake.
#' @param site_dictionary a data frame with the lake, obs_type, site_id,
#'                        SWIMS_station, USGS_id, WBIC, and static_iso_type of
#'                        measurement sites, as in the
#'                        \code{\link{site_dictionary}} dataset, subset for a
#'                        single lake.
#' @param stage_vol a data frame with the lake, stage_m, surf_area_m2, and
#'                  volume_m3 as in the \code{\link{stage_vol}} dataset, subset
#'                  for a single lake.
#' @param wind_elev height of wind measurement, defaults to 3m for Hancock station.
#' @param z0 aeorodynamic roughness of landcover at site of weather measurements
#'          (m), defaults to 0.02 for a grass.
#'
#' @return monthly_weather, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{atmp_K}{mean monthly air temperature (K)}
#' \item{RH_pct}{mean monthly relative humidity (percent)}
#' \item{P_mm}{monthly precipitaiton (mm)}
#' \item{ET_mm}{monthly lake evaporation (mm)}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @importFrom lubridate floor_date
#' @importFrom NISTunits NISTdegCtOk NISTdegTOradian
#' @importFrom faoET McJannet_lake_ET
#'
#' @seealso \code{\link{retrieve_csls_weather}}, \code{\link{weather}}
#'
#' @export
get_monthly_weather <- function(weather, lst, stage_vol, site_dictionary,
                                wind_elev = 3, z0 = 0.02){
  # Extract location information for this lake
  loc     <- site_dictionary %>%
             filter(.data$obs_type == "LK") %>%
             select(z = .data$elev_m,
                    phi = .data$lat_deg,
                    Lm = .data$long_deg)
  loc$phi <- NISTdegTOradian(loc$phi)
  loc$Lm  <- loc$Lm
  loc$Lz  <- 90
  loc     <- as.list(loc)

  # Extract initial lake surface temperature for this lake
  day1   <- floor_date(lst$date[which.min(lst$date)], unit = "day")
  wtmp0  <- lst$ltmp[which.min(lst$date)]

  # Summarize at daily weather
  daily_weather <- weather %>%
                   group_by(datetimes = floor_date(.data$date, unit = "day")) %>%
                   summarise(atmp_min = min(.data$atmp),
                             atmp_max = max(.data$atmp),
                             RH_min = min(.data$RH),
                             RH_max = max(.data$RH),
                             P = sum(.data$P),
                             Rs = sum(.data$Rs),
                             wind = mean(.data$wind))
  daily_weather <- daily_weather %>%
                   filter(.data$datetimes > day1)

  # Format inputs for lake evap calculations, calculate lake evap
  weather           <- daily_weather %>%
                       select(.data$datetimes, .data$Rs, .data$wind) %>%
                       as.list()
  weather$atmp      <- list(min = daily_weather$atmp_min,
                            max = daily_weather$atmp_max)
  weather$RH        <- list(min = daily_weather$RH_min,
                            max = daily_weather$RH_max)
  weather$wind_elev <- wind_elev
  weather$dt        <- "daily"
  weather$z0        <- z0
  weather$wtmp0     <- wtmp0

  lake              <- list(A = 1e-6*max(stage_vol$surf_area_m2),
                            depth_m = max(stage_vol$depth_m))

  albedo            <- list(lake = 0.08)

  atmp <- (weather$atmp$min + weather$atmp$max)/2
  RH   <- (weather$RH$min + weather$RH$max)/2
  ET   <- McJannet_lake_ET(loc, lake, weather, albedo)

  weather <- as.data.frame(cbind(daily_weather$datetimes, atmp, RH,
                                 daily_weather$P, ET))
  colnames(weather) <- c("date", "atmp", "RH", "P", "ET")
  weather$date <- as_datetime(weather$date)

  # Summarize weather at a monthly timestep
  monthly_weather <- weather %>%
                     group_by(date = floor_date(.data$date, unit = "month")) %>%
                     summarise(atmp_K = NISTdegCtOk(mean(.data$atmp)),
                               RH_pct = mean(.data$RH),
                               P_mm = sum(.data$P),
                               ET_mm = sum(.data$ET))
  return(monthly_weather)
}
