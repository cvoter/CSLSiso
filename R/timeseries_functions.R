# timeseries_functions.R
# Functions to calculate starting date, ending date, and number of months in a
# monthly timeseries.
# Includes:
# - fill_timeseries_gaps
# - find_overlap_timeseries
# - find_start_date
# - find_end_date
# - find_nmonths

# ------------------------------------------------------------------------------
#' Fill Gaps in Summary Data
#'
#' This function compares dates in a given (complete) timeseries and dataframe,
#' then adds blank rows for the dates that are missing in the dataframe.
#'
#' @param df data frame with a "date" column
#' @param timeseries vector with complete list of dates that should be in the
#'                   "date" column of df
#'
#' @return df, the same data frame with blank rows added to fill in timeseries,
#'        then sorted by date.
#'
#' @import lubridate
#'
#' @export

fill_timeseries_gaps <- function (df, timeseries){
  no_dates <- timeseries$dates[which(!timeseries$dates %in% df$date)]
  no_dates <- as_datetime(no_dates)
  if (length(no_dates) > 0) {
    for (i in 1:length(no_dates)){
      df[nrow(df)+1,]     <- NA
      df$date[nrow(df)]   <- no_dates[i]
    }
  }
  df <- df %>% arrange(.data$date)
  return(df)
}

# ------------------------------------------------------------------------------
#' Identify Months With Overlapping Data
#'
#' Identifies the start date, end date, and number of months in the monthly
#' timeseries with complete input data coverage.
#'
#' @inheritParams summarise_inputs
#'
#' @import lubridate
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select group_by summarise
#' @importFrom rlang .data
#'
#' @export

find_timeseries <- function(isotopes, lake_levels = NULL, gw_levels = NULL,
                            dictionary = NULL, threshold = 0.01,
                            by_gw_iso = FALSE){
  if (by_gw_iso){
    isotopes           <- iso_site_type(isotopes, dictionary, lake_levels,
                                        gw_levels, threshold)
    analysis_dates     <- isotopes %>%
                          filter(.data$site_type == "upgradient") %>%
                          group_by(floor_date(.data$date, unit = "month")) %>%
                          summarise(date = floor_date(mean(.data$date), unit = "day")) %>%
                          select(.data$date) %>%
                          unlist()
    analysis_dates     <- as_datetime(analysis_dates)
    analysis_intervals <- interval(analysis_dates %m-% months(1), analysis_dates)
  } else {
    start_date     <- find_start_date(isotopes$date)
    end_date       <- find_end_date(isotopes$date)
    nmonths        <- find_nmonths(start_date, end_date)
    analysis_dates <- NULL
    for (i in 1:nmonths){
      analysis_dates[i] <- start_date + months(i-1)
    }
    analysis_dates <- as_datetime(analysis_dates)
    analysis_intervals <- interval(analysis_dates,
                                   analysis_dates + months(1) - days(1))
    analysis_dates     <- analysis_dates + months(1) - days(1)
  }

  return(list(dates = analysis_dates,
              intervals = analysis_intervals))
}

# ------------------------------------------------------------------------------
#' Identify Start Date of Timeseries
#'
#' Identifies the starting date of a monthly timeseries (forced to the 1st of
#' the month)
#'
#' @param date_vector vector with dates in timeseries
#' @param all_days logical defaults to FALSE to indicate that timeseries does
#'                 not need to include values for every day within the month,
#'                 set to TRUE to force this behavior.
#'
#' @return
#' \describe{
#' \item{start_date}{first useable date in the timeseries (rounded to the first
#'                   of the month)}
#' }
#'
#' @import lubridate
#'
#' @export
find_start_date <- function(date_vector, all_days = FALSE){
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
#' Identify End Date of Timeseries
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
find_end_date <- function(date_vector, all_days = FALSE){
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
#' Identify Number of Months in Timeseries
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
find_nmonths <- function(start_date, end_date){
  date_interval <- interval(start = start_date, end = end_date)
  nmonths       <- round(time_length(date_interval, unit = "month"))
  return(nmonths)
}
