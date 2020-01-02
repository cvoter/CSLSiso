#' Classify Groundwater Stable Isotope Measurements (Dynamic)
#'
#' Classifies the site type of isotope measurements as precipitation, lake,
#' upgradient, or downgradient. Upgradient vs. downgradient wells are determined
#' by comparing the daily difference in lake levels and well levels in the 30
#' days leading up to isotope measurements.
#'
#' @inheritParams summarise_isotopes
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select arrange
#' @import lubridate
#' @importFrom rlang .data
#' @importFrom bit64 as.integer64
#' @importFrom stats median
#'
#' @export

iso_site_type <- function(isotopes, dictionary, lake_levels = NULL,
                          gw_levels = NULL, threshold = 0.01) {

  # Identify sites with a static isotope site type
  iso_sites                  <- dictionary %>%
                                filter(is.na(.data$static_iso_class) == FALSE)
  iso_sites$site_id          <- as.character(iso_sites$site_id)
  iso_sites$static_iso_class <- as.character(iso_sites$static_iso_class)

  # Identify which sites are groundwater sites
  gw_sites <- dictionary %>%
              filter(.data$obs_type == "GW",
                     .data$static_iso_class != "invalid")

  # Get all gw-lk level differences
  for (i in 1:nrow(gw_levels)) {
    today <- gw_levels$date[i]
    gw    <- gw_levels$level_m[i]
    lk    <- lake_levels$level_m[lake_levels$date == today]
    if (length(lk) > 0) {
      gw_levels$diff_m[i] <- gw - lk
    } else {
      gw_levels$diff_m[i] <- NA
    }
  }

  # Classify measurements (upgradient, downgradient, precipitation, lake)
  isotopes$site_type <- NA
  for (i in 1:nrow(isotopes)) {
    if (isotopes$site_id[i] %in% gw_sites$site_id) {
      # GROUNDWATER
      # Filter well level records to just this well for this month
      this_USGS_id <- gw_sites %>%
                      filter(as.character(.data$site_id) ==
                               as.character(isotopes$site_id[i])) %>%
                      select(.data$USGS_id) %>%
                      as.numeric()
      date_meas    <- isotopes$date[i]
      this_well    <- gw_levels %>%
                      filter(.data$site_no == this_USGS_id,
                             .data$date <= isotopes$date[i],
                             .data$date >= (isotopes$date[i] %m-% months(1)))
      if (length(this_well$diff_m) > 0) {
        # Use lake-gw water level difference, if have
        if (abs(median(this_well$diff_m, na.rm = TRUE)) < threshold) {
          isotopes$site_type[i] <- NA
        } else if (mean(this_well$diff_m, na.rm = TRUE) > 0) {
          isotopes$site_type[i] <- "upgradient"
        } else {
          isotopes$site_type[i] <- "downgradient"
        }
      } else if (as.character(isotopes$site_id[i]) %in% iso_sites$site_id) {
        # Use static site types if don't have lake-gw water level difference
        isotopes$site_type[i] <- iso_sites %>%
                                 filter(as.character(.data$site_id) ==
                                          isotopes$site_id[i]) %>%
                                 select(.data$static_iso_class) %>%
                                 as.character()
      }

    } else if (isotopes$site_id[i] == "PRECIP") {
      # PRECIPITATION
      isotopes$site_type[i] <- "precipitation"

    } else if (isotopes$site_id[i] %in%
               dictionary$site_id[which(dictionary$obs_type == "LK")]) {
      # LAKE
      isotopes$site_type[i] <- "lake"
    }
  } #end loop through isotope measurements
  return(isotopes)
}
