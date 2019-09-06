#' Daily to Monthly Weather
#' 
#' This function takes daily max air temperature, min air temperature, max
#' relative humidity, min relative humidity, precipitaiton, and reference 
#' evapotranspiration as formatted by the Hancock, WI weather station and 
#' summarizes it at a monthly time step.
#' 
#' @references \url{https://enviroweather.msu.edu/weather.php?stn=hck}
#'
#' @param filename name of daily weather file which includes:
#' \itemize{
#'   \item date (m/d/YYYY)
#'   \item atmp_max - maximum air temperature (deg C)
#'   \item atmp_min - minimum air temperature (deg C)
#'   \item relh_max - maximum relative humidity (%)
#'   \item relh_min - minimum relative humidity (%)
#'   \item pcpn - precipitation (mm)
#'   \item rpet - reference potential evapotranspiration (mm/day)
#' }
#' @param filedir directory in which daily weather csv file resides
#' @param skip_lines number of rows in csv to skip before reading, default: 7
#' 
#' @return monthly_weather, a data frame with the following columns:
#' \describe{
#' \item{month}{month of observation (m)}
#' \item{year}{year of observation (YYYY)}
#' \item{atmp_K}{mean monthly air temperature (K)}
#' \item{relh_pct}{mean monthly relative humidity (percent)}
#' \item{pcpn_mm}{monthly precipitaiton (mm)}
#' \item{rpet_mm}{monthly reference potential evapotranspiration (mm)}
#' }
#' 
#' @importFrom lubridate day month year
#' @importFrom utils read.csv
#' 
#' @export

daily_to_monthly_weather <- function(filename,
                                     filedir = 'data',
                                     skip_lines = 7) {
  # Load daily weather data, trim comments off end
  fileloc       <- sprintf('%s/%s', filedir, filename)
  daily_weather <- read.csv(fileloc, skip = skip_lines)
  end_data      <- which(daily_weather$date == "Variable Ids:") - 2
  daily_weather <- daily_weather[1:end_data, ]
  
  # Convert factors to numeric
  daily_weather$atmp_max <- as.numeric(as.character(daily_weather$atmp_max))
  daily_weather$atmp_min <- as.numeric(as.character(daily_weather$atmp_min))
  
  # Identify first and last months
  daily_weather$date <- as.Date(daily_weather$date, format = "%m/%d/%Y")
  ndays              <- nrow(daily_weather)
  
  start_day     <- day(daily_weather$date[1])
  start_month   <- month(daily_weather$date[1])
  start_year    <- year(daily_weather$date[1])
  if (start_day != 1) {
    start_month <- start_month + 1
    start_day   <- 1
  }
  
  end_day     <- day(daily_weather$date[ndays])
  end_month   <- month(daily_weather$date[ndays])
  end_year    <- year(daily_weather$date[ndays])
  if (end_day < 28) {
    end_month <- end_month - 1
  }
  
  # Calculate total number of months in time series
  if (end_year == start_year) {
    nmonths <- end_month - start_month + 1
  } else {
    months_yr1  <- 12 - start_month + 1
    months_yr2  <- end_month
    months_btwn <- end_year - start_year - 1
    nmonths     <- months_yr1 + months_yr2 + months_btwn
  }
  
  # Extract monthly weather
  monthly_weather <- NULL
  for (i in 1:nmonths) {
    # Set month and year
    if (i == 1) {
      m <- start_month
      y  <- start_year
    } else {
      m <- monthly_weather$month[i-1] + 1
      y  <- monthly_weather$year[i-1]
      if (m == 13) {
        m <- 1
        y  <- monthly_weather$year[i-1] + 1
      }
    }
    monthly_weather$month[i] <- m
    monthly_weather$year[i]  <- y
    
    # Summarize for the month
    this_weather <- daily_weather[which(year(daily_weather$date) == y &
                                          month(daily_weather$date) == m),]
    
    monthly_weather$atmp_K[i]   <- mean(c(this_weather$atmp_max, 
                                          this_weather$atmp_min),
                                        na.rm = TRUE) + 273.15
    monthly_weather$relh_pct[i] <- mean(c(this_weather$relh_max, 
                                          this_weather$relh_min),
                                        na.rm = TRUE)
    monthly_weather$pcpn_mm[i]  <- sum(this_weather$pcpn, na.rm = TRUE)
    monthly_weather$rpet_mm[i]  <- sum(this_weather$rpet, na.rm = TRUE)
  } #end loop through months
  
  monthly_weather <- as.data.frame(monthly_weather)
  
  return(monthly_weather)
  
}