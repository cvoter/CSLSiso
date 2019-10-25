#' Calculate Lake Evaporation
#'
#' Calculates Lake Evaporation
#'
#' @inheritParams summarise_weather
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
#' @importFrom faoET lake_evap
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr group_by summarise
#'
#' @export
calculate_lake_evap <- function(weather, lst, elev_area_vol, dictionary,
                                Lz = 90, lake_albedo = 0.08, wind_elev = 3,
                                z0 = 0.02, no_condensation = FALSE) {
  # Location info for lake evaporation
  loc <- format_lake_loc(dictionary, Lz)

  # Lake info for lake evaporation
  daily_lst  <- lst %>%
                group_by(date = floor_date(.data$date, unit = "day")) %>%
                summarise(ltmp = mean(.data$ltmp, na.rm = TRUE))
  lake       <- format_lake_info(elev_area_vol, daily_lst)

  # Determine first day with both lst and weather data
  lst_day_zero    <- floor_date(lst$date[which.min(lst$date)], unit = "day")
  weather_day_zero <- floor_date(weather$date[which.min(weather$date)], unit = "day")
  day_zero         <- max(c(lst_day_zero, weather_day_zero))

  # Weather info for lake evaporation
  weather       <- format_lake_weather(weather, day_zero, wind_elev, z0)
  weather$wtmp0 <- daily_lst$ltmp[daily_lst$date == day_zero]
  lake$lst      <- lake$lst[-c(which(lake$lst$date == day_zero)), ]

  # Lake water albedo for lake evaporation
  albedo <- list(lake = lake_albedo)

  # Lake evaporation
  E <- lake_evap(loc, lake, weather, albedo, no_condensation)

  # Combine daily weather values into new weather dataframe
  weather <- as.data.frame(cbind(weather$datetimes, weather$atmp$min,
                                 weather$atmp$max, weather$RH$min,
                                 weather$RH$max, weather$P, E))
  colnames(weather) <- c("date", "atmp_min", "atmp_max", "RH_min", "RH_max",
                         "P", "E")
  weather$date <- as_datetime(weather$date)

  return(weather)
}
