#' Classify Stable Isotope Measurements (Static)
#'
#' Classifies the site type of isotope measurements as precipitation, lake,
#' upgradient, or downgradient. Upgradient vs. downgradient wells are determined
#' by static definitions in the site dictionary.
#'
#' @param isotopes a data frame with the following columns
#' \itemize{
#' \item date - date and time of sample collection (Date)
#' \item site_id - unique site id for where measurement was taken (e.g.,
#'                 LL-01)
#' \item d18O - stable isotope measurement for d18O at this site
#' \item d2H - stable isotope measurement for for duterium at this site
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
#'
#' @return isotopes - the same data frame provided to the function, but with an
#'                    additional column for "site_type" with values of
#'                    "precipitation", "lake", "upgradient", "downgradient", or
#'                    NA.
#'
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom lubridate days
#' @importFrom rlang .data
#'
#' @export

classify_iso_site_static <- function(isotopes,
                                     lake,
                                     site_file = "csls_site_dictionary.csv",
                                     filedir = 'system.file') {

  # Sites - load dictionary and subset to sites related to this lake
  if (filedir == 'system.file') {
    site_dictionary <- read.csv(system.file("extdata",
                                            site_file,
                                            package = "isoH2Obudget",
                                            mustWork = TRUE))
  } else {
    site_dictionary <- read.csv(sprintf("%s/%s", filedir, site_file))
  }
  iso_sites                  <- site_dictionary %>%
                                filter(as.character(.data$lake) == !!lake,
                                       is.na(.data$static_iso_class) == FALSE)
  iso_sites$site_id          <- as.character(iso_sites$site_id)
  iso_sites$static_iso_class <- as.character(iso_sites$static_iso_class)
  isotopes$site_id           <- as.character(isotopes$site_id)

  # Classify measurements (upgradient, downgradient, precipitation, lake)
  isotopes$site_type <- NA
  for (i in 1:nrow(isotopes)) {
    if (isotopes$site_id[i] %in% iso_sites$site_id) {
      this_type             <- iso_sites %>%
                               filter(.data$site_id == isotopes$site_id[i]) %>%
                               select(.data$static_iso_class) %>%
                               as.character()
      isotopes$site_type[i] <- this_type
    }
  }
  return(isotopes)
}
