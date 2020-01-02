#' Monthly Lake Surface Temperature
#'
#' Summarizes sub-monthly lake surface temperature at a monthly timestep for a
#' single lake
#'
#' @inheritParams summarise_inputs
#' @param timeseries a list with dates (POSIXct) to analyze, and intervals
#'                 (lubridate interval) covering the month before each analysis
#'                 date.
#'
#' @return monthly_lst, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{ltmp_degC}{mean lake surface temperature (degrees K)}
#' \item{ltmp_K}{mean lake surface temperature (degrees K)}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @import lubridate
#' @importFrom NISTunits NISTdegCtOk
#'
#' @export
summarise_lst <- function(lst, timeseries){
  monthly_lst <- NULL
  for (i in 1:length(timeseries$intervals)) {
    these_lst                <- lst %>%
                                filter(.data$date %within% timeseries$intervals[i])
    monthly_lst$date[i]      <- timeseries$dates[i]
    monthly_lst$ltmp_degC[i] <- mean(these_lst$ltmp, na.rm = TRUE)
    monthly_lst$ltmp_K[i]    <- NISTdegCtOk(monthly_lst$ltmp_degC[i])
    if (is.nan(monthly_lst$ltmp_degC[i])) {
      monthly_lst$ltmp_degC[i] <- NA
      monthly_lst$ltmp_K[i]    <- NA
    }
  }
  monthly_lst$date <- as_datetime(monthly_lst$date)
  monthly_lst      <- as.data.frame(monthly_lst)
  return(monthly_lst)
}
