#' Fill Gaps in Stable Isotope Measurements
#'
#' Linearly interolates lake and GW values for dates in timeseries with missing
#' measurements. Depending on arguments, fills in precipitation values using 1)
#' measurments collected that month, and 2) Maribeth Kniffin's data, also from
#' Hancock station.
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

iso_gapfill <- function(monthly_isotopes) {
  # 1. Lake & GW: Interpolate NAs
  zoo_iso                     <- read.zoo(monthly_isotopes, index.name = "date")
  zoo_iso                     <- as.data.frame(na.approx(zoo_iso, rule = 2))
  monthly_isotopes$d18O_lake  <- zoo_iso$d18O_lake
  monthly_isotopes$d2H_lake   <- zoo_iso$d2H_lake
  monthly_isotopes$d18O_GWin  <- zoo_iso$d18O_GWin
  monthly_isotopes$d2H_GWin   <- zoo_iso$d2H_GWin
  monthly_isotopes$d18O_GWout <- zoo_iso$d18O_GWout
  monthly_isotopes$d2H_GWout  <- zoo_iso$d2H_GWout

  # 2. Precip: Use CSLS pcpn measurement for entire month
  for (i in 1:nrow(monthly_isotopes)) {
    this_month   <- month(monthly_isotopes$date[i])
    this_i       <- which(month(monthly_isotopes$date) == this_month)

    # Fill in with this month, if exists
    monthly_isotopes$d18O_pcpn[i] <- mean(monthly_isotopes$d18O_pcpn[this_i],
                                          na.rm = TRUE)
    monthly_isotopes$d2H_pcpn[i] <- mean(monthly_isotopes$d2H_pcpn[this_i],
                                         na.rm = TRUE)

    # Replace NaNs with NA if that didn't work
    monthly_isotopes$d18O_pcpn[is.nan(monthly_isotopes$d18O_pcpn)] <- NA
    monthly_isotopes$d2H_pcpn[is.nan(monthly_isotopes$d2H_pcpn)]   <- NA
  }

  # 3. Precip: Average in Maribeth Kniffin values
  kniffin_isotopes <- isoH2Obudget::kniffin[["isotopes"]]
  for (i in 1:nrow(monthly_isotopes)) {
    # Average in Maribeth Kniffin data from same location
    this_month    <- monthly_isotopes$date[i]
    kniffin_month <- kniffin_isotopes %>%
      filter(month(.data$date) == month(this_month))
    if (is.na(monthly_isotopes$d18O_pcpn[i])){
      monthly_isotopes$d18O_pcpn[i] <- kniffin_month$d18O_pcpn
      monthly_isotopes$d2H_pcpn[i]  <- kniffin_month$d2H_pcpn
    }
  }

  return(monthly_isotopes)
}
