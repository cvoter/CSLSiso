#' Classify Stable Isotope Measurements (Dynamic)
#'
#' Classifies the site type of isotope measurements as precipitation, lake,
#' upgradient, or downgradient. Upgradient vs. downgradient wells are determined
#' by comparing the 7-day mean lake level and well level leading up to isotope
#' measurements.
#'
#' @param isotopes a data frame with the following columns
#' \itemize{
#' \item date - date and time of sample collection (Date)
#' \item site_id - unique site id for where measurement was taken (e.g.,
#'                 LL-01)
#' \item d18O - stable isotope measurement for d18O at this site
#' \item d2H - stable isotope measurement for for duterium at this site
#' }
#' @param water_levels a data frame with the following columns
#' \itemize{
#' \item date - date of water level measurement
#' \item site_no - UGSG site number
#' \item obs_type - type of observation (LK = lake level, GW = groundwater
#'                  level)
#' \item level_m - water level in meters above mean sea level
#' }
#' @param lake name of lake to analyze (e.g., Pleasant, Long, or Plainfield).
#'             This name corresponds to lake names in the site dictionary "lake"
#'             column.
#' @param site_file filname of site dictionary with the following columns
#' \itemize{
#' \item lake - name of lake associated with the measurement site. Corresponds
#'              to lake name argument in this function.
#' \item obs_type - type of observation (LK = lake, GW = groundwater
#'                  monitoring well)
#' \item site_id - unique site id for measurement site (e.g., LL-01).
#'                 Corresponds to site_id in "isotopes" data frame.
#' \item SWIMS_station_id - SWIMS station id, if exists for this site.
#' \item USGS_id - USGS site number. Corresponds to site_no in "water_levels"
#'                 data frame.
#' \item WBIC - water body identification code (WBIC), for lake sites only.
#' }
#' @param filedir name of directory with both csv files, defaults to
#'                'system.file' to instruct it to look within package
#'                "inst/extdata" directory.
#' @param Xday number of days to consider when calculating average water level
#'             before isotope measurements, defaults to 7 days.
#'
#' @return isotopes - the same data frame provided to the function, but with an
#'                    additional column for "site_type" with values of
#'                    "precipitation", "lake", "upgradient", or "downgradient".
#'
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom lubridate days
#' @importFrom rlang .data
#' @importFrom bit64 as.integer64
#'
#' @export

classify_iso_site_dynamic <- function(isotopes,
                                      water_levels,
                                      lake,
                                      site_file = "csls_site_dictionary.csv",
                                      filedir = 'system.file',
                                      Xday = 7) {

  # Sites - load dictionary and subset to sites related to this lake
  if (filedir == 'system.file') {
    site_dictionary <- read.csv(system.file("extdata",
                                            site_file,
                                            package = "isoH2Obudget",
                                            mustWork = TRUE))
  } else {
    site_dictionary <- read.csv(sprintf("%s/%s", filedir, site_file))
  }
  all_sites    <- site_dictionary %>%
                  filter(.data$lake == !!lake)
  gw_sites     <- site_dictionary %>%
                  filter(.data$lake == !!lake,
                         .data$obs_type == "GW")
  lake_USGS_id <- all_sites %>%
                  filter(.data$obs_type == "LK") %>%
                  select(.data$USGS_id) %>%
                  as.numeric() %>%
                  as.integer64()

  # Water levels - subset to records related to this lake
  lake_levels <- water_levels %>%
                 filter(.data$site_no == lake_USGS_id,
                        .data$obs_type == "LK")
  well_levels <- water_levels %>%
                 filter(.data$site_no %in% c(gw_sites$USGS_id),
                        .data$obs_type == "GW")

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
      this_well    <- well_levels %>%
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
               all_sites$site_id[which(all_sites$obs_type == "LK")]) {
      isotopes$site_type[i] <- "lake"
    } #end loop through isotope measurements
  }
  return(isotopes)
}
