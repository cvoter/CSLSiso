#' Monthly Weather
#'
#' Summarizes sub-monthly weather at a monthly timestep
#'
#' @inheritParams summarise_inputs
#' @param timeseries a list with dates (POSIXct) to analyze, and intervals
#'                 (lubridate interval) covering the month before each analysis
#'                 date.
#' @param wind_elev height of wind measurement, defaults to 3m for Hancock
#'                  station.
#' @param z0 aeorodynamic roughness of landcover at site of weather measurements
#'          (m), defaults to 0.02 for a grass.
#' @param Lz the longitude of the local timezone (degrees west of Greenwich,
#'           ranges from 0 to 360 degrees). Defaults to 90 for Central Time
#'           Zone, USA.
#' @param no_condensation logical defaults to FALSE to include days with
#'                         condensation in daily lake evaporation calculations.
#'                         If TRUE, sets all days with condensation (i.e.,
#'                         negative lake evaporation) to zero.
#'
#' @return monthly_weather, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{atmp_K}{mean monthly air temperature (K)}
#' \item{RH_pct}{mean monthly relative humidity (percent)}
#' \item{P_mm}{monthly precipitaiton (mm)}
#' \item{ET_mm}{monthly lake evaporation (mm)}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @importFrom lubridate floor_date
#' @importFrom NISTunits NISTdegCtOk NISTdegTOradian
#'
#' @export
summarise_weather <- function(weather, lst, elev_area_vol, dictionary,
                              timeseries, wind_elev = 3, z0 = 0.02, Lz = 90,
                              no_condensation = FALSE){

  # Get lake evap, ET
  # In the process converts hourly weather to daily
  # (atmp -> min/max daily atmp and RH -> min/max daily RH)
  daily_weather <- calculate_lake_evap(weather, lst, elev_area_vol, dictionary,
                                       Lz, wind_elev, z0,
                                       no_condensation)

  # # Ensure months have complete data for summing
  # start_date <- find_start_date(daily_weather$date, all_days = TRUE)
  # end_date   <- find_end_date(daily_weather$date, all_days = TRUE)
  # daily_weather <- daily_weather %>%
  #                  filter(.data$date >= start_date,
  #                         .data$date <= end_date)

  # Summarize weather at a monthly timestep
  monthly_weather <- NULL
  for (i in 1:length(timeseries$intervals)) {
    this_weather <- daily_weather %>%
                    filter(.data$date %within% timeseries$intervals[i])

    atmp_min <- mean(this_weather$atmp_min, na.rm = TRUE)
    atmp_max <- mean(this_weather$atmp_max, na.rm = TRUE)
    RH_min   <- mean(this_weather$RH_min, na.rm = TRUE)
    RH_max   <- mean(this_weather$RH_max, na.rm = TRUE)

    monthly_weather$date[i]      <- timeseries$dates[i]
    monthly_weather$atmp_degC[i] <- mean(c(atmp_min, atmp_max))
    monthly_weather$atmp_K[i]    <- NISTdegCtOk(monthly_weather$atmp_degC[i])
    monthly_weather$RH_pct[i]    <- mean(c(RH_min, RH_max))
    monthly_weather$P_mm[i]      <- sum(this_weather$P)
    monthly_weather$E_mm[i]      <- sum(this_weather$E)
  }
  monthly_weather$date <- as_datetime(monthly_weather$date)
  monthly_weather      <- as.data.frame(monthly_weather)

  return(monthly_weather)
}
