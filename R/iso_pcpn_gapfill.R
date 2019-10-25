#' Fill Gaps in Precipitation Stable Isotope Measurements
#'
#' Fills in precipitation isotope measurements using 1) measurments collected in
#' subsequent month, assuming collector was active during month with missing
#' data, and 2) Maribeth Kniffin's data, also from Hancock station.
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
#' @importFrom rlang .data
#' @import lubridate
#'
#' @export

iso_pcpn_gapfill <- function(monthly_isotopes, extend_pcpn = TRUE,
                             use_kniffin_pcpn = TRUE){
  # 1. Extend measurements to previous months if collector was active
  if (extend_pcpn) {
    pcpn_iso_active <- isoH2Obudget::pcpn_iso_active
    new_isotopes <- monthly_isotopes
    for (i in 1:nrow(new_isotopes)) {
      this_month <- month(new_isotopes$date[i])
      this_i     <- which(month(new_isotopes$date) == this_month)
      next_month <- month(new_isotopes$date[i] %m+% months(1))
      next_i     <- which(month(new_isotopes$date) == next_month)

      # Fill in with this month, if exists
      new_isotopes$d18O_pcpn[i] <- mean(monthly_isotopes$d18O_pcpn[this_i],
                                        na.rm = TRUE)
      new_isotopes$d2H_pcpn[i] <- mean(monthly_isotopes$d2H_pcpn[this_i],
                                       na.rm = TRUE)

      # Replace NaNs with NA if that didn't work
      new_isotopes$d18O_pcpn[is.nan(new_isotopes$d18O_pcpn)] <- NA
      new_isotopes$d2H_pcpn[is.nan(new_isotopes$d2H_pcpn)]   <- NA

      # # Replace NAs with next month, if pcpn collector was active
      # if ((is.na(new_isotopes$d18O_pcpn[i])) & (length(next_i) > 0) &
      #     any(new_isotopes$date[i] %within% pcpn_iso_active)) {
      #   new_isotopes$d18O_pcpn[i] <- mean(monthly_isotopes$d18O_pcpn[next_i],
      #                                         na.rm = TRUE)
      #   new_isotopes$d2H_pcpn[i]  <- mean(monthly_isotopes$d2H_pcpn[next_i],
      #                                         na.rm = TRUE)
      # }
      # # Replace NaNs with NA if that didn't work
      # new_isotopes$d18O_pcpn[is.nan(new_isotopes$d18O_pcpn)] <- NA
      # new_isotopes$d2H_pcpn[is.nan(new_isotopes$d2H_pcpn)]   <- NA
    }
    monthly_isotopes <- new_isotopes
  }

  # 2. Average in Maribeth Kniffin values
  if (use_kniffin_pcpn) {
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
  }
  return(monthly_isotopes)
}
