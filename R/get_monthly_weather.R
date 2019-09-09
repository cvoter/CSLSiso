#' Get Monthly Weather
#'
#' This function summarizes daily or subdaily weather at a monthly timestep.
#' Required weather fields include air temperature (min/max if daily, deg C),
#' relative humidity (min/max if daily, %), precipitation (mm), and reference
#' evapotranspiration (mm). Assumes column names and units are as received from
#' the Hancock, WI weather station.
#'
#' @references \url{https://enviroweather.msu.edu/weather.php?stn=hck}
#'
#' @param filename name of weather csv file which includes:
#' \itemize{
#'   \item date (m/d/YYYY)
#'   \item (atmp_max and atmp_min) OR atmp - min/max air temperature (daily, deg
#'         C) OR air temperature (subdaily, deg C)
#'   \item (relh_max and relh_min) OR relh - min/max relative humidity (daily,
#'         %) OR relative humidity (subdaily, %)
#'   \item pcpn - precipitation (mm)
#'   \item rpet - reference potential evapotranspiration (mm)
#' }
#' @param filedir directory in which weather csv file resides, defaults to
#'                'system.file' to instruct it to look within package
#'                "inst/extdata" directory.
#' @param skip_lines [numeric] number of rows in csv to skip before reading in
#'                   data, defaults to 7.
#' @param daily_weather [logical] defaults to FALSE for subdaily weather, set
#'                      to TRUE for daily weather
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

get_monthly_weather <- function(filename,
                                filedir = 'system.file',
                                skip_lines = 7,
                                daily_weather = FALSE) {
  # Load weather data
  if (filedir == 'system.file') {
    weather <- read.csv(system.file("extdata",
                                  filename,
                                  package = "isoH2Obudget",
                                  mustWork = TRUE),
                        skip = skip_lines)
  } else {
    weather <- read.csv(sprintf('%s/%s', filedir, filename), skip = skip_lines)
  }

  # Fix formatting
  end_data         <- which(weather$date == "Variable Ids:") - 2
  weather          <- weather[1:end_data, ]
  if (daily_weather) {
    weather$atmp_max <- as.numeric(as.character(weather$atmp_max))
    weather$atmp_min <- as.numeric(as.character(weather$atmp_min))
  } else {
    weather$atmp <- as.numeric(as.character(weather$atmp))
  }

  # Identify start month and total number of months
  weather$date <- as.Date(weather$date, format = "%m/%d/%Y")
  month_info   <- start_n_months(weather$date, all_days = TRUE)

  # Extract monthly weather
  monthly_weather <- NULL
  for (i in 1:month_info$nmonths) {
    this_month <- month_info$start_date + months(i-1)
    m          <- month(this_month)
    y          <- year(this_month)

    # Summarize for the month
    this_weather <- weather[which(year(weather$date) == y &
                                    month(weather$date) == m),]
    monthly_weather$date[i]     <- this_month
    if (daily_weather) {
      monthly_weather$atmp_K[i]   <- mean(c(this_weather$atmp_max,
                                            this_weather$atmp_min),
                                          na.rm = TRUE) + 273.15
      monthly_weather$relh_pct[i] <- mean(c(this_weather$relh_max,
                                            this_weather$relh_min),
                                          na.rm = TRUE)
    } else {
      monthly_weather$atmp_K[i]   <- mean(this_weather$atmp, na.rm = TRUE) +
                                     273.15
      monthly_weather$relh_pct[i] <- mean(this_weather$relh, na.rm = TRUE)
    }
    monthly_weather$pcpn_mm[i]  <- sum(this_weather$pcpn, na.rm = TRUE)
    monthly_weather$rpet_mm[i]  <- sum(this_weather$rpet, na.rm = TRUE)
  }

  # R bizzarly looses the class of date objects in for loops, fix here
  monthly_weather$date <- as.Date(monthly_weather$date, origin = "1970-01-01")

  return(as.data.frame(monthly_weather))

}
