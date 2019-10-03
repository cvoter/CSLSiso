#' Retrieve Hourly Central Sands Weather
#'
#' This function retrieves hourly weather from the Hancock, WI weather station.
#' Required weather fields include air temperature (deg C), relative humidity
#' (%), precipitation (mm), and reference evapotranspiration (mm). NA values
#' filled in via linear interpolation.
#'
#' @param filename name of Hancock weather csv
#' @param filedir directory in which weather csv file resides, defaults to
#'                'system.file' to instruct it to look within installed package
#'                files.
#' @param skip_lines [numeric] number of rows in csv to skip before reading in
#'                   data, defaults to 7.
#'
#' @return weather, a data frame with the following columns for hourly weather:
#' \describe{
#' \item{date}{date and time of weather observation}
#' \item{atmp}{air temperature (deg C)}
#' \item{P}{precipitation (mm)}
#' \item{RH}{relative humidity (percent)}
#' \item{Rs}{incoming solar or shortwave radiation (MJ/m^2/hr)}
#' \item{wind}{wind speed (m/s)}
#' }
#'
#' @importFrom lubridate mdy_hm
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom zoo read.zoo na.approx
#'
#' @export

retrieve_csls_weather <- function(filename,
                                  filedir = "system.file",
                                  skip_lines = 7){
  # Load weather data
  weather      <- load_pkg_csv(filename, filedir, skip_lines)

  # Fix formatting
  end_data     <- which(weather$date == "Variable Ids:") - 2
  weather      <- weather[1:end_data, ]
  weather$date <- as.character(weather$date)
  weather$time <- as.character(weather$time)
  weather$atmp <- as.numeric(as.character(weather$atmp))
  weather$srad <- weather$srad/1000 #kJ to MJ

  # Fix times
  weather$time[which(weather$time == "24:00:00")] <- "24:00"
  weather$date <- mdy_hm(sprintf("%s %s", weather$date, weather$time))

  # Select columns of interest
  weather <- weather %>%
             select(.data$date,
                    .data$atmp,
                    .data$pcpn,
                    .data$relh,
                    .data$srad,
                    .data$wspd)
  colnames(weather) <- c("date","atmp","P","RH","Rs","wind")

  # Interpolate NAs
  zoo.weather   <- read.zoo(weather)
  zoo.weather   <- as.data.frame(na.approx(zoo.weather))
  weather[,2:6] <- zoo.weather

  return(weather)
}
