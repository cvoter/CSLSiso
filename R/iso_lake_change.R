#' Estimates change in lake isotope measurement since previous month
#'
#' Calculates change in d18O and d2H in the lake over the previous 30 days.
#'
#' @param monthly_isotopes a data frame with monthly isotope measurements for
#'                         all dates of desired timeseries (with NAs for months
#'                         with no measurements)
#'
#' @return monthly_isotopes - the same data frame provided to the function, but
#'                            with delta_d18O_lake and delta_d2H_lake values
#'                            filled in for all months (except maybe the very
#'                            first).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select summarise
#' @import lubridate
#' @importFrom rlang .data
#' @importFrom NISTunits NISTsecTOday
#'
#' @export

iso_lake_change <- function(monthly_isotopes) {

  monthly_isotopes <- monthly_isotopes[order(monthly_isotopes$date),]

  monthly_isotopes$delta_d18O_lake <- 0
  monthly_isotopes$delta_d2H_lake  <- 0

  for (i in 2:nrow(monthly_isotopes)){
    date1    <- monthly_isotopes$date[i-1]
    date2    <- monthly_isotopes$date[i]
    duration <- NISTsecTOday(int_length(interval(date1, date2)))

    d18O_lake1 <- monthly_isotopes$d18O_lake[i-1]
    d18O_lake2 <- monthly_isotopes$d18O_lake[i]

    d2H_lake1 <- monthly_isotopes$d2H_lake[i-1]
    d2H_lake2 <- monthly_isotopes$d2H_lake[i]

    delta_d18O_lake <- 30*(d18O_lake2 - d18O_lake1)/duration
    delta_d2H_lake  <- 30*(d2H_lake2 - d2H_lake1)/duration

    monthly_isotopes$delta_d18O_lake[i] <- delta_d18O_lake
    monthly_isotopes$delta_d2H_lake[i]  <- delta_d2H_lake
  }

  return(monthly_isotopes)
}
