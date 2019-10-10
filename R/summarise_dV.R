#' Monthly Change in Lake Volume
#'
#' Calculates the monthly change in lake volume based on daily lake levels and a
#' stage-volume relationship from bathymetry analysis. .
#'
#' @param lake_levels a data frame with daily water level measurements as
#'                    formatted in the \code{\link{water_levels}} dataset,
#'                    subset to lake level records for the lake of interest.
#' @param timeseries a vector of all months in the common timeseries among input
#'                   datasets
#'
#' @return monthly_dV, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{dV}{change in lake volume during the month as a depth (mm)}
#' }
#'
#' @import lubridate
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom rlang .data
#'
#' @export

summarise_dV <- function(lake_levels, timeseries) {
  monthly_dV <- NULL
  for (i in 1:length(timeseries)) {
    this_month  <- timeseries[i]

    daily_stage <- lake_levels %>%
                   filter(month(.data$date) == month(this_month),
                          year(.data$date) == year(this_month)) %>%
                   mutate(stage_m = .data$level_m) %>%
                   select(.data$date, .data$stage_m)
    stage_start <- daily_stage %>%
                   filter(.data$date == min(.data$date)) %>%
                   select(.data$stage_m) %>%
                   as.numeric()
    stage_end   <- daily_stage %>%
                   filter(.data$date == max(.data$date)) %>%
                   select(.data$stage_m) %>%
                   as.numeric()
    dV          <- (stage_end - stage_start)*1000 # m to mm

    monthly_dV$date[i] <- this_month
    monthly_dV$dV[i]   <- dV
  }

  # R bizzarly looses the class of date objects in for loops, fix here
  monthly_dV$date <- as_datetime(monthly_dV$date)

  return(as.data.frame(monthly_dV))
}
