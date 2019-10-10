#' Format Lake Weather Info for Lake Evaporation
#'
#' Takes hourly weather data, summarizes as daily weather data, and re-formats
#' for input to lake evaporation functions
#'
#' @param weather hourly weather data with air temperature (atmp), relative
#'                humidity (RH), precipitation (P), solar radiaiton (Rs), and
#'                wind speed (wind) as formatted in the \code{\link{weather}
#'                dataset.
#' @param day_one first day of weather info to retain
#' @param wind_elev height of wind measurements (m). At Hancock, 3m.
#' @param z0 aeorodynamic roughness of landcover at site of weather measurements
#'          (m). For a grass, 0.02m.
#'
#' @return weather, a list with daily weather data that includes:
#' \describe{
#' \item{datetimes}{datetimes of weather records [POSIXct]}
#' \item{P}{vector of daily precipitation (mm) corresponding with datetimes
#'          vector}
#' \item{atmp}{list with two vectors, "min" and "max", with mean daily min and
#'             max air temperature (degrees C) corresponding with datetimes
#'             vector}
#' \item{RH}{list with two vectors, "min" and "max", with mean daily min and
#'            max relative humidity (percent) corresponding with datetimes
#'            vector.}
#' \item{Rs}{vector of incoming solar or shortwave radiation (MJ/m^2/timestep),
#'           corresponding with datetimes vector.}
#' \item{wind}{vector with mean windspead (m/s), corresponding with datetimes
#'             vector}
#' \item{wind_elev}{height at which wind is measured (m)}
#' \item{dt}{string indicating the timestep of input weather series. Expects
#'           "hourly", "daily", or "monthly".}
#' \item{z0}{aerodynamic roughness of measurement site (m)}
#'  }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @importFrom NISTunits NISTdegTOradian
#'
#' @seealso \code{\link{retrieve_csls_weather}},
#'          \code{\link{weather}}
#'
#' @export
format_lake_weather <- function(weather, day_one, wind_elev, z0) {
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
                   filter(.data$datetimes > day_one)

  # Convert to list for input to lake evap function (minus lake temp info)
  weather           <- daily_weather %>%
                       select(.data$datetimes, .data$P, .data$Rs, .data$wind) %>%
                       as.list()
  weather$atmp      <- list(min = daily_weather$atmp_min,
                            max = daily_weather$atmp_max)
  weather$RH        <- list(min = daily_weather$RH_min,
                            max = daily_weather$RH_max)
  weather$wind_elev <- wind_elev
  weather$dt        <- "daily"
  weather$z0        <- z0

  return(weather)
}
