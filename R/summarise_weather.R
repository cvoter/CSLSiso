#' Monthly Weather
#'
#' Summarizes sub-monthly weather at a monthly timestep
#'
#' @param weather a data frame with sub-monthly weather including date, atmp (air
#'                temperature, deg C), RH (relative humidity, percent), P
#'                (precipitation, mm), Rs (incoming solar radiation, MJ/m^2),
#'                and wind (wind speed, m/s) as formatted in the
#'                weather dataset.
#' @param lst a data frame with sub-monthly lake surface temperature
#'            measurements as formatted in the lst dataset, subset
#'            for a single lake.
#' @param site_dictionary a data frame with the lake, obs_type, site_id,
#'                        SWIMS_station, USGS_id, WBIC, and static_iso_type of
#'                        measurement sites, as in the
#'                        site_dictionary dataset, subset for a
#'                        single lake.
#' @param timeseries a vector of all months in the common timeseries among input
#'                   datasets
#' @param stage_vol a data frame with the lake, stage_m, surf_area_m2, and
#'                  volume_m3 as in the stage_vol dataset, subset
#'                  for a single lake.
#' @param wind_elev height of wind measurement, defaults to 3m for Hancock station.
#' @param z0 aeorodynamic roughness of landcover at site of weather measurements
#'          (m), defaults to 0.02 for a grass.
#' @param Lz the longitude of the local timezone (degrees west of Greenwich,
#'           ranges from 0 to 360 degrees). Defaults to 90 for Central Time
#'           Zone, USA.
#' @param lake_albedo albedo of the lake, defaults to 0.08 for open water.
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
#'
#' @export
summarise_weather <- function(weather, lst, stage_vol, site_dictionary,
                              timeseries, wind_elev = 3, z0 = 0.02, Lz = 90,
                              lake_albedo = 0.08){

  # Get lake evap, ET
  # In the process converts hourly weather to daily
  # (atmp -> min/max daily atmp and RH -> min/max daily RH)
  daily_weather <- calculate_lake_evap(weather, lst, stage_vol,
                                       site_dictionary, Lz, lake_albedo)

  # Ensure months have complete data for summing
  start_date <- find_start_date(daily_weather$date, all_days = TRUE)
  end_date   <- find_end_date(daily_weather$date, all_days = TRUE)
  daily_weather <- daily_weather %>%
                   filter(.data$date >= start_date,
                          .data$date <= end_date)

  # Summarize weather at a monthly timestep
  monthly_weather        <- daily_weather %>%
                            group_by(date = floor_date(.data$date,
                                                       unit = "month")) %>%
                            filter(.data$date %in% timeseries) %>%
                            summarise(atmp_degC = mean(c(mean(.data$atmp_min),
                                                         mean(.data$atmp_max))),
                                      RH_pct = mean(c(mean(.data$RH_min),
                                                      mean(.data$RH_max))),
                                      P_mm = sum(.data$P),
                                      ET_mm = sum(.data$ET))
  monthly_weather$atmp_K <- NISTdegCtOk(monthly_weather$atmp_degC)

  monthly_weather <- fill_timeseries_gaps(monthly_weather, timeseries)

  return(monthly_weather)
}
