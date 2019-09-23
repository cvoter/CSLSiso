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
#' @param Xday number of days to consider when calculating average water level
#'             before isotope measurements, defaults to 7 days.
#'
#' @return isotopes - the same data frame provided to the function, but with an
#'                    additional column for "site_type" with values of
#'                    "precipitation", "lake", "upgradient", or "downgradient".
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom lubridate days
#' @importFrom rlang .data
#' @importFrom bit64 as.integer64
#'
#' @export

classify_iso_site_dynamic <- function(isotopes,
                                      lake_levels,
                                      gw_levels,
                                      site_dictionary,
                                      Xday = 7) {
  # Identify which sites are groundwater sites
  gw_sites <- site_dictionary %>%
              filter(.data$obs_type == "GW")

  # Classify measurements (upgradient, downgradient, precipitation, lake)
  isotopes$site_type <- NA
  for (i in 1:nrow(isotopes)) {
    if (isotopes$site_id[i] %in% gw_sites$site_id) {
      # Filter well level records to just this well
      this_USGS_id <- gw_sites %>%
                      filter(as.character(.data$site_id) ==
                               as.character(isotopes$site_id[i])) %>%
                      select(.data$USGS_id) %>%
                      as.numeric()
      this_well    <- gw_levels %>%
                      filter(.data$site_no == this_USGS_id)

      # X day mean lake level before isotope measurement
      lake_meas_i <- which.min(abs(lake_levels$date - isotopes$date[i]))
      lake_Xday   <- lake_levels$date[lake_meas_i] - days(Xday)
      lake_Xday_i <- which.min(abs(lake_levels$date - lake_Xday))
      lake_mean_m <- mean(lake_levels$level_m[lake_Xday_i:lake_meas_i])

      # X day mean well level before isotope measurement
      well_meas_i <- which.min(abs(this_well$date - isotopes$date[i]))
      well_Xday   <- this_well$date[well_meas_i] - days(Xday)
      well_Xday_i <- which.min(abs(this_well$date - well_Xday))
      well_mean_m <- mean(this_well$level_m[well_Xday_i:well_meas_i])

      # Compare well and lake water levels
      if (well_mean_m > lake_mean_m) {
        isotopes$site_type[i] <- "upgradient"
      } else {
        isotopes$site_type[i] <- "downgradient"
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
