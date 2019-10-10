#' Calculate Lake Evaporation
#'
#' Calculates Lake Evaporation
#'
#' @param
#'
#'
#' @return
#'
#' @importFrom faoET McJannet_lake_ET
#'
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
