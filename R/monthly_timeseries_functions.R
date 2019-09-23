# monthly_timeseries_functions.R
# Functions to calculate starting date, ending date, and number of months in a
# monthly timeseries.
# Includes:
# - get_overlap_months
# - get_start_date
# - get_end_date
# - get_nmonths
# ------------------------------------------------------------------------------
#' Identify Months With Overlapping Data
#'
#' Identifies the start date, end date, and number of months in the monthly
#' timeseries with complete input data coverage.
#'
#' @param weather a data frame with daily weather in the following columns:
#' \itemize{
#' \item date - date and time of weather observation
#' \item atmp - air temperature (deg C)
#' \item pcpn - precipitation (mm)
#' \item relh - relative humidity (percent)
#' \item rpet - reference potential evapotranspiration (mm)
#' }
#' @param lst a data frame with every-other-week lake surface temperature info:
#' \itemize{
#' \item WBIC - Water Body Identification Code for lake
#' \item date - date and time of observation
#' \item ltmp - lake surface temperature (degrees C)
#' \item units - units of lake surface temperature, should all be "DEGREES C"
#' }
#' @param isotopes a data frame with isotope measurements structured as follows:
#' \itemize{
#' \item date - date of measurement
#' \item lake - lake associated with measurement
#' \item site_id - unique ID for site of measurement, e.g. "PRECIP", "LONG",
#'                "LL-01"
#' \item d18O - isotopic composition for 18O (per mil)
#' \item d2H - isotopic composition for duterium (per mil)
#' }
#' @param lake_levels a data frame with daily lake levels structured as follows:
#' \itemize{
#' \item date - date of water level measurement
#' \item site_no - UGSG site number
#' \item obs_type - type of observation (LK = lake level)
#' \item level_m - water level in meters above mean sea level
#' }
#' @param gw_levels a data frame with daily groundwater levels structured as
#'                  follows:
#' \itemize{
#' \item date - date of water level measurement
#' \item site_no - UGSG site number
#' \item obs_type - type of observation (GW = groundwater level)
#' \item level_m - water level in meters above mean sea level
#' }
#'
#' @return a list with the following variables:
#' \describe{
#' \item{start_date}{start date (1st of the month) of timeseries with
#'                   overlapping input data}
#' \item{end_date}{end date (last day of the month) of timeseries with
#'                 overlapping input data}
#' \item{nmonths}{number of months in timeseries with overlapping input data}
#' }
#'
#' @export

get_overlap_months <- function(weather, lst, isotopes, lake_levels, gw_levels){
  start_weather  <- get_start_date(weather$date, all_days = TRUE)
  start_lst      <- get_start_date(lst$date)
  start_isotopes <- get_start_date(isotopes$date)
  start_lake     <- get_start_date(lake_levels$date, all_days = TRUE)
  start_gw       <- get_start_date(gw_levels$date)
  start_date     <- max(start_weather,
                        start_lst,
                        start_isotopes,
                        start_lake,
                        start_gw)

  end_weather  <- get_end_date(weather$date, all_days = TRUE)
  end_lst      <- get_end_date(lst$date)
  end_isotopes <- get_end_date(isotopes$date)
  end_lake     <- get_end_date(lake_levels$date, all_days = TRUE)
  end_gw       <- get_end_date(gw_levels$date)
  end_date     <- min(end_weather,
                      end_lst,
                      end_isotopes,
                      end_lake,
                      end_gw)

  nmonths <- get_nmonths(start_date, end_date)

  return(list(start_date = start_date, end_date = end_date, nmonths = nmonths))
}

# ------------------------------------------------------------------------------
#' Identify Start Date
#'
#' Identifies the starting date of a monthly timeseries (forced to the 1st of
#' the month)
#'
#' @param date_vector vector with dates in timeseries
#' @param all_days [logical] defaults to FALSE to indicate that timeseries does
#'                 not need to include values for every day within the month,
#'                 set to TRUE to force this behavior.
#'
#' @return start_date first useable date in the timeseries (rounded to the first
#'         of the month)
#'
#' @import lubridate
#'
#' @export
get_start_date <- function(date_vector, all_days = FALSE){
  start_date   <- min(date_vector)
  prev_date    <- start_date - days(1)
  if (month(start_date) == month(prev_date) & all_days == TRUE) {
    start_date <- floor_date(start_date, unit = "month") + months(1)
  } else if (all_days == FALSE) {
    start_date <- floor_date(start_date, unit = "month")
  }
  return(start_date)
}

# ------------------------------------------------------------------------------
#' Identify End Date
#'
#' Identifies the ending date of a monthly timeseries (forced to the last day of
#' the month)
#'
#' @param date_vector vector with dates in timeseries
#' @param all_days [logical] defaults to FALSE to indicate that timeseries does
#'                 not need to include values for every day within the month,
#'                 set to TRUE to force this behavior.
#'
#' @return end_date last useable date in the timeseries (rounded to the last day
#'         of the month)
#'
#' @importFrom lubridate days month floor_date ceiling_date
#'
#' @export
get_end_date <- function(date_vector, all_days = FALSE){
  end_date   <- max(date_vector)
  next_date  <- end_date + days(1)
  if (month(end_date) == month(next_date) & all_days == TRUE) {
    end_date <- floor_date(end_date, unit = "month") - days(1)
  } else if (all_days == FALSE) {
    end_date <- ceiling_date(end_date, unit = "month") - days(1)
  }
  return(end_date)
}

# ------------------------------------------------------------------------------
#' Number of Months in Timeseries
#'
#' Calculates the number of months in a timeseries given a start and end date.
#'
#' @param start_date date of first month in time series (assumes day = first day
#'                   of that month)
#' @param end_date date of last month in time series (assumes day = last day of
#'                 that month)
#'
#' @return nmonths - number of months in this time series
#'
#' @importFrom lubridate interval time_length
#'
#' @export
get_nmonths <- function(start_date, end_date){
  date_interval <- interval(start = start_date, end = end_date)
  nmonths       <- round(time_length(date_interval, unit = "month"))
  return(nmonths)
}
