#' Record Groundwater Sites Used for Isotopes
#'
#' Identifies which groundwater wells are included in monthly averages of
#' groundwater stable isotope measurements, then makes note of this in the
#' summary dataframe.
#'
#' @param monthly_isotopes a data frame with monthly isotope measurements for
#'                         all dates of desired timeseries (with NAs for months
#'                         with no measurements)
#' @param isotopes a data frame with isotope measurements as formatted in the
#'                \code{\link{isotopes}} dataset, subset to records for the lake
#'                of interest.
#'
#' @return monthly_isotopes - same data frame with GWin_sites and GWout_sites
#'                            added as columns
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter ungroup select
#' @importFrom rlang .data
#' @import lubridate
#'
#' @export

iso_gw_site_names <- function(monthly_isotopes, isotopes) {
  GWin_sites  <- rep("", length(monthly_isotopes$date))
  GWout_sites <- rep("", length(monthly_isotopes$date))
  for (i in 1:nrow(monthly_isotopes)) {
    # Note GWin sites, if exist
    if (is.na(monthly_isotopes$d18O_GWin[i]) == FALSE) {
      tmp           <- isotopes %>%
                       filter(floor_date(.data$date, unit = "month") ==
                                monthly_isotopes$date[i],
                              .data$site_type == "upgradient") %>%
                       summarise(site_ids = str_c(unique(.data$site_id),
                                                  collapse = ", "))
      GWin_sites[i] <- tmp$site_ids
    }
    # Note GWout sites, if exist
    if (length(monthly_isotopes$d18O_GWout) > 0){
      if (is.na(monthly_isotopes$d18O_GWout[i]) == FALSE) {
        tmp            <- isotopes %>%
                          filter(floor_date(.data$date, unit = "month") ==
                                   monthly_isotopes$date[i],
                                 .data$site_type == "downgradient") %>%
                          summarise(site_ids = str_c(unique(.data$site_id),
                                                     collapse = ", "))
        GWout_sites[i] <- tmp$site_ids
      }
    }
  }
  monthly_isotopes$GWin_sites  <- GWin_sites
  monthly_isotopes$GWout_sites <- GWout_sites
  return(monthly_isotopes)
}
