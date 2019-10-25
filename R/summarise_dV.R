#' Monthly Change in Lake Volume
#'
#' Calculates the monthly change in lake volume based on daily lake levels and
#' an elevation-volume relationship from bathymetry analysis.
#'
#' @inheritParams summarise_inputs
#' @param analysis a list with dates (POSIXct) to analyze, and intervals
#'                 (lubridate interval) covering the month before each analysis
#'                 date.
#'
#' @return monthly_dV, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{dV}{change in lake volume during the month (m^3)}
#' \item{mean_vol_m3}{mean lake volume during the month (m^3)}
#' \item{mean_area_m2}{mean lake surface area during the month (m^2)}
#' }
#'
#' @import lubridate
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom rlang .data
#'
#' @export

summarise_dV <- function(lake_levels, analysis) {
  monthly_dV <- NULL
  for (i in 1:length(analysis$dates)) {
    these_levels <- lake_levels %>%
                    filter(.data$date %within% analysis$intervals[i])
    vol_start <- these_levels %>%
                 filter(.data$date == min(.data$date)) %>%
                 select(.data$vol_m3) %>%
                 as.numeric()
    vol_end   <- these_levels %>%
                 filter(.data$date == max(.data$date)) %>%
                 select(.data$vol_m3) %>%
                 as.numeric()
    vol_mean  <- mean(these_levels$vol_m3)
    area_mean <- mean(these_levels$area_m2)

    monthly_dV$date[i]         <- analysis$dates[i]
    monthly_dV$dV_m3[i]        <- vol_end - vol_start
    monthly_dV$dV_mm[i]        <- 1000*monthly_dV$dV_m3[i]/area_mean
    monthly_dV$mean_vol_m3[i]  <- vol_mean
    monthly_dV$mean_area_m2[i] <- area_mean
  }

  # R bizzarly looses the class of date objects in for loops, fix here
  monthly_dV$date <- as_datetime(monthly_dV$date)

  return(as.data.frame(monthly_dV))
}
