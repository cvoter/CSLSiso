#' Retrieve Central Sands Isotope Measurements
#'
#' This function loads isotope measurements for the CSLS lakes and extracts
#' isotopic measurements for 18O and 2H (duterium).
#'
#' @param filename name of isotope csv file with stable isotope measurement
#'                 info, including (but not limited to):
#' \itemize{
#'   \item Lake - lake associated with measurement
#'   \item Site.ID - unique site id for where location was taken (e.g. LL-01 for
#'                   Long Lake well 1). Assumed to match site ids in sites_file.
#'   \item Valid - field indicating whether or not sample measurement is valid
#'                 for analysis (TRUE) or not (FALSE).
#'   \item d18O..VSMOW. - stable isotope measurement for d18O at this site
#'   \item dD..VSMOW. - stable isotope measurement for d18O at this site
#'   \item Collection.Date.Time - date and time of sample collection
#' }
#' @param filedir name of directory with both csv files, defaults to
#'                'system.file' to instruct it to look within package
#'                "inst/extdata" directory.
#'
#' @return isotopes, a data frame with the following columns:
#' \describe{
#'   \item{date}{date of measurement}
#'   \item{lake}{lake associated with measurement}
#'   \item{site_id}{unique ID for site of measurement, e.g. "LL-01"}
#'   \item{d18O}{isotopic composition for 18O}
#'   \item{d2H}{isotopic composition for duterium}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#' @importFrom lubridate mdy_hm
#'
#' @export

retrieve_csls_isotopes <- function(filename, filedir = 'system.file') {
  isotopes           <- load_pkg_csv(filename, filedir)
  isotopes           <- isotopes %>%
                        filter(.data$Valid == TRUE,
                               is.na(.data$d18O..VSMOW.) == FALSE) %>%
                        select(.data$Collection.Date.Time,
                               .data$Lake,
                               .data$Site.ID,
                               .data$d18O..VSMOW.,
                               .data$dD..VSMOW.)
  colnames(isotopes) <- c("date", "lake", "site_id", "d18O", "d2H")
  isotopes$date      <- mdy_hm(isotopes$date)

  return(isotopes)
}
