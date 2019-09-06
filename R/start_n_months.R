#' Monthly Timeseries Start and Number
#'
#' This function takes a date vector and determines the start month, start
#' year, and number of months in a time series. It includes an option to
#' require a complete daily timeseries in order to include a month.
#'
#' @param date_vector a vector of class "Date"
#' @param all_days logical. If TRUE, timeseries start and end must include all
#'                 days within the month (default = TRUE)
#'
#' @return month_info, a list
#' \describe{
#' \item{start_date}{Start month of timeseries (YYYY-MM-01)}
#' \item{nmonths}{number of months in timeseries}
#' }
#'
#' @import lubridate
#'
#' @export

start_n_months <- function(date_vector, all_days = TRUE) {

  # Start month
  start_date   <- min(date_vector)
  prev_date    <- start_date - days(1)
  if (month(start_date) == month(prev_date) & all_days == TRUE) {
    start_date <- floor_date(start_date, unit = "month") + months(1)
  } else if (all_days == FALSE) {
    start_date <- floor_date(start_date, unit = "month")
  }

  # End month
  end_date   <- max(date_vector)
  next_date  <- end_date + days(1)
  if (month(start_date) == month(next_date) & all_days == TRUE) {
    end_date <- floor_date(end_date, unit = "month") - days(1)
  } else if (all_days == FALSE) {
    end_date <- ceiling_date(end_date, unit = "month") - days(1)
  }

  # Number of months in time series
  date_interval <- interval(start = start_date, end = end_date)
  nmonths       <- round(time_length(date_interval, unit = "month"))

  return(list(start_date = start_date, nmonths = nmonths))
}
