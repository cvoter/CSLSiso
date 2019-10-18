#' Fill Gaps in Precipitation Stable Isotope Measurements
#'
#' Fills in precipitation isotope measurements using 1) measurments collected in
#' subsequent month, assuming collector was active during month with missing
#' data, and 2) Maribeth Kniffin's data, also from Hancock station.
#'
#' @param monthly_isotopes a data frame with monthly isotope measurements for
#'                         all dates of desired timeseries (with NAs for months
#'                         with no measurements)
#' @param use_kniffin_pcpn logical defaults to TRUE to average in precipitation
#'                         measurements by Maribeth Kniffin for each month.
#' @param file name of file with dates of precipitation collector deployment.
#'             Defaults to "csls_isotope_precipitation_deployment.csv"
#' @param filedir name of directory with file with dates of precipitation
#'                collector deployment. Defaults to "system.file" to indicate is
#'                stored within inst/extdata within the installed isoH2Obudget
#'                package files.
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

iso_pcpn_gapfill <- function(monthly_isotopes, use_kniffin_pcpn = TRUE,
                             file = 'csls_isotope_precipitation_deployment.csv',
                             filedir = "system.file"){
  # 1. Extend measurements to previous months if collector was active
  deployment            <- load_pkg_csv(file, filedir)
  deployment$start_date <- floor_date(mdy_hm(deployment$start_date), unit = "month")
  deployment$end_date   <- floor_date(mdy_hm(deployment$end_date), unit = "month")
  if (is.na(deployment$end_date[nrow(deployment)])) {
    deployment$end_date[nrow(deployment)] <- today()
  }
  intervals             <- interval(deployment$start_date, deployment$end_date)

  for (i in 1:nrow(monthly_isotopes)) {
    this_month <- monthly_isotopes$date[i]
    next_month <- this_month + months(1)
    next_i     <- which(monthly_isotopes$date == next_month)
    if ((is.na(monthly_isotopes$d18O_pcpn[i])) & (length(next_i) > 0) &
        any(monthly_isotopes$date[i] %within% intervals)) {
      monthly_isotopes$d18O_pcpn[i] <- monthly_isotopes$d18O_pcpn[next_i]
      monthly_isotopes$d2H_pcpn[i] <- monthly_isotopes$d2H_pcpn[next_i]
    }
  }

  # 2. Average in Maribeth Kniffin values
  if (use_kniffin_pcpn) {
    kniffin_isotopes <- NULL
    data(kniffin_isotopes, envir = environment())
    for (i in 1:nrow(monthly_isotopes)) {
      # Average in Maribeth Kniffin data from same location
      this_month   <- monthly_isotopes$date[i]
      kniffin_pcpn <- kniffin_isotopes %>%
                      filter(month(.data$date) == month(this_month))
      monthly_isotopes$d18O_pcpn[i] <- mean(c(monthly_isotopes$d18O_pcpn[i],
                                              kniffin_pcpn$d18O_pcpn),
                                            na.rm = TRUE)
      monthly_isotopes$d2H_pcpn[i] <- mean(c(monthly_isotopes$d2H_pcpn[i],
                                              kniffin_pcpn$d2H_pcpn),
                                            na.rm = TRUE)
    }
  }
  return(monthly_isotopes)
}
