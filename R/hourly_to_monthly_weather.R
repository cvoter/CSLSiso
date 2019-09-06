#' Hourly to Monthly Weather
#'
#' This function takes hourly air temperature, relative humidity,
#' precipitaiton, and reference evapotranspiration as formatted by the Hancock,
#'  WI weather station and summarizes it at a monthly time step.
#'
#' @references \url{https://enviroweather.msu.edu/weather.php?stn=hck}
#'
#' @param filename name of hourly weather file which includes:
#' \itemize{
#'   \item date (m/d/YYYY)
#'   \item time (H:MM)
#'   \item atmp - air temperature (deg C)
#'   \item relh - relative humidity (%)
#'   \item pcpn - precipitation (mm)
#'   \item rpet - reference potential evapotranspiration (mm/hr)
#' }
#' @param filedir directory in which hourly weather csv file resides
#' @param skip_lines number of rows in csv to skip before reading, default: 7
#'
#' @return monthly_weather, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation (Date)}
#' \item{atmp_K}{mean monthly air temperature (K)}
#' \item{relh_pct}{mean monthly relative humidity (percent)}
#' \item{pcpn_mm}{monthly precipitaiton (mm)}
#' \item{rpet_mm}{monthly reference potential evapotranspiration (mm)}
#' }
#'
#' @import lubridate
#' @importFrom utils read.csv
#'
#' @export

hourly_to_monthly_weather <- function(filename,
                                      filedir = 'data',
                                      skip_lines = 7) {
  # Load hourly weather data
  fileloc        <- sprintf('%s/%s', filedir, filename)
  hourly_weather <- read.csv(fileloc, skip = skip_lines)

  # Fix formatting
  end_data            <- which(hourly_weather$date == "Variable Ids:") - 2
  hourly_weather      <- hourly_weather[1:end_data, ]
  hourly_weather$atmp <- as.numeric(as.character(hourly_weather$atmp))

  # Identify start month and total number of months
  hourly_weather$date <- as.Date(hourly_weather$date, format = "%m/%d/%Y")
  month_info          <- start_n_months(hourly_weather$date, all_days = TRUE)

  # Extract monthly weather
  monthly_weather <- NULL
  for (i in 1:month_info$nmonths) {
    this_month <- month_info$start_date + months(i-1)
    m          <- month(this_month)
    y          <- year(this_month)

    this_weather <- hourly_weather[which(year(hourly_weather$date) == y &
                                           month(hourly_weather$date) == m),]
    monthly_weather$date[i]     <- this_month
    monthly_weather$atmp_K[i]   <- mean(this_weather$atmp, na.rm = TRUE) +
                                   273.15
    monthly_weather$relh_pct[i] <- mean(this_weather$relh, na.rm = TRUE)
    monthly_weather$pcpn_mm[i]  <- sum(this_weather$pcpn, na.rm = TRUE)
    monthly_weather$rpet_mm[i]  <- sum(this_weather$rpet, na.rm = TRUE)
  }

  # R bizzarly looses the class of date objects in for loops, fix here
  monthly_weather$date <- as.Date(monthly_weather$date, origin = "1970-01-01")

  return(as.data.frame(monthly_weather))
}
