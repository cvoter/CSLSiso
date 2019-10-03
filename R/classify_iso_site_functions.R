# classify_iso_site_functions.R
# Functions to summarize input data at a monthly timestep.
# Includes:
# - classify_iso_site_static
# - classify_iso_site_dynamic

# ------------------------------------------------------------------------------
#' Classify Stable Isotope Measurements (Static)
#'
#' Classifies the site type of isotope measurements as precipitation, lake,
#' upgradient, or downgradient. Upgradient vs. downgradient wells are determined
#' by static definitions in the site dictionary.
#'
#' @param isotopes a data frame with isotope measurements as formatted in the
#'                \code{\link{isotopes}} dataset, subset to records for the lake
#'                of interest.
#' @param site_dictionary a data frame with site id numbers and static isotope
#'                        site classifications as formatted in the
#'                        \code{\link{site_dictionary}} dataset, subset to
#'                        records for the lake of interest.
#'
#' @return isotopes - the same data frame provided to the function, but with an
#'                    additional column for "site_type" with values of
#'                    "precipitation", "lake", "upgradient", "downgradient", or
#'                    "".
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#'
#' @export

classify_iso_site_static <- function(isotopes,
                                     site_dictionary) {
  # Subset sites to those with a static isotope site type
  iso_sites                  <- site_dictionary %>%
                                filter(is.na(.data$static_iso_class) == FALSE)
  iso_sites$site_id          <- as.character(iso_sites$site_id)
  iso_sites$static_iso_class <- as.character(iso_sites$static_iso_class)
  isotopes$site_id           <- as.character(isotopes$site_id)

  # Classify measurements (upgradient, downgradient, precipitation, lake)
  isotopes$site_type <- NA
  for (i in 1:nrow(isotopes)) {
    if (isotopes$site_id[i] %in% iso_sites$site_id) {
      isotopes$site_type[i] <- iso_sites %>%
                               filter(.data$site_id == isotopes$site_id[i]) %>%
                               select(.data$static_iso_class) %>%
                               as.character()
    }
  }
  return(isotopes)
}

# ------------------------------------------------------------------------------
#' Classify Stable Isotope Measurements (Dynamic)
#'
#' Classifies the site type of isotope measurements as precipitation, lake,
#' upgradient, or downgradient. Upgradient vs. downgradient wells are determined
#' by comparing the 7-day mean lake level and well level leading up to isotope
#' measurements.
#'
#' @param isotopes a data frame with isotope measurements as formatted in the
#'                \code{\link{isotopes}} dataset, subset to records for the lake
#'                of interest.
#' @param lake_levels a data frame with daily water level measurements as
#'                    formatted in the \code{\link{water_levels}} dataset,
#'                    subset to lake level records for the lake of interest.
#' @param gw_levels a data frame with daily water level measurements as
#'                  formatted in the \code{\link{water_levels}} dataset,
#'                  subset to groundwater level records at the lake of
#'                  interest.
#' @param site_dictionary a data frame with site id numbers as formatted in the
#'                        \code{\link{site_dictionary}} dataset, subset to
#'                        records for the lake of interest.
#' @param median_threshold minimum median difference between lake levels and
#'                         groundwater levels during the month of measurement in
#'                         order to classify groundwater measurement.
#'
#' @return isotopes - the same data frame provided to the function, but with an
#'                    additional column for "site_type" with values of
#'                    "precipitation", "lake", "upgradient", or "downgradient".
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select arrange
#' @import lubridate
#' @importFrom rlang .data
#' @importFrom bit64 as.integer64
#' @importFrom stats median
#'
#' @export

classify_iso_site_dynamic <- function(isotopes,
                                      lake_levels,
                                      gw_levels,
                                      site_dictionary,
                                      median_threshold = 0.03) {
  # Identify which sites are groundwater sites
  gw_sites <- site_dictionary %>%
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

      # Classify well
      if (length(this_well$diff_m) > 0) {
        if (abs(median(this_well$diff_m, na.rm = TRUE)) < median_threshold) {
          isotopes$site_type[i] <- NA
        } else if (mean(this_well$diff_m, na.rm = TRUE) > 0) {
          isotopes$site_type[i] <- "upgradient"
        } else {
          isotopes$site_type[i] <- "downgradient"
        }
      }

    } else if (isotopes$site_id[i] == "PRECIP") {
      isotopes$site_type[i] <- "precipitation"
    } else if (isotopes$site_id[i] %in%
               site_dictionary$site_id[which(site_dictionary$obs_type == "LK")]) {
      isotopes$site_type[i] <- "lake"
    } #end loop through isotope measurements
  }
  return(isotopes)
}
