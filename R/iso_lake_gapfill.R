#' Fill Gaps in Lake Stable Isotope Measurements
#'
#' Either uses the fall lake static isotope measurement for all periods of the
#' year (static_lake  = TRUE) or linearly interolates values for months with
#' missing measurements.
#'
#' @param monthly_isotopes a data frame with monthly isotope measurements for
#'                         all dates of desired timeseries (with NAs for months
#'                         with no measurements)
#' @inheritParams summarise_isotopes
#'
#' @return monthly_isotopes - the same data frame provided to the function, but
#'                            with d18O_lake values filled in for all months
#'                            (except maybe the very first and last months).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select summarise
#' @import lubridate
#' @importFrom rlang .data
#' @importFrom zoo read.zoo na.approx
#'
#' @export

iso_lake_gapfill <- function(monthly_isotopes, static_lake = FALSE) {
  # If static lake is turned on, set all to fall
  if (static_lake) {
    fall_d18O_lake <- monthly_isotopes %>%
                      filter(month(.data$date) %in% c(9,10,11,12)) %>%
                      select(.data$d18O_lake) %>%
                      summarise(fall_d18O_lake =
                                  mean(.data$d18O_lake, na.rm = T)) %>%
                      unlist()
    fall_d2H_lake <- monthly_isotopes %>%
                     filter(month(.data$date) %in% c(9,10,11,12)) %>%
                     select(.data$d2H_lake) %>%
                     summarise(fall_d2H_lake =
                                 mean(.data$d2H_lake, na.rm = T)) %>%
                     unlist()
    if (is.na(fall_d18O_lake) == FALSE) {
      monthly_isotopes$d18O_lake <- fall_d18O_lake
      monthly_isotopes$d2H_lake  <- fall_d2H_lake
    }
  }

  # Interpolate NAs in lake isotopes
  zoo_iso                    <- read.zoo(monthly_isotopes, index.name = "date")
  zoo_iso                    <- as.data.frame(na.approx(zoo_iso, rule = 2))
  monthly_isotopes$d18O_lake <- zoo_iso$d18O_lake
  monthly_isotopes$d2H_lake <- zoo_iso$d2H_lake

  return(monthly_isotopes)
}
