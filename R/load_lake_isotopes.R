#' Lake Isotope Measurements
#'
#' This function extracts isotopic measurements for 18O and duterium for a
#' specified lake.
#'
#' @param lake name of lake to get isotopic measurments for
#' @param isotope_file name of isotope csv file with stable isotope measurement
#'                     info, including (but not limited to):
#' \itemize{
#' \item Lake - lake associated with measurement
#' \item Site.ID - unique site id for where location was taken (e.g. LL-01 for
#'      Long Lake well 1). Assumed to match site ids in sites_file.
#' \item Valid - field indicating whether or not sample measurement is valid
#'       for analysis (TRUE) or not (FALSE).
#' \item d18O..VSMOW. - stable isotope measurement for d18O at this site
#' \item dD..VSMOW. - stable isotope measurement for d18O at this site
#' \item Collection.Date.Time - date and time of sample collection
#' }
#' @param filedir name of directory with both csv files, defaults to
#'                'system.file' to instruct it to look within package
#'                "inst/extdata" directory.
#'
#' @return lake_isotopes, a data frame with the following columns:
#' \describe{
#' \item{date}{date of measurement}
#' \item{site_id}{unique ID for site of measurement}
#' \item{d18O}{isotopic composition for 18O}
#' \item{dD}{isotopic composition for duterium}
#' }
#'
#' @import lubridate
#' @importFrom utils read.csv
#'
#' @export

load_lake_isotopes <- function(lake = "Pleasant",
                          isotope_file,
                          filedir = 'system.file') {
  # Load data
  if (filedir == 'system.file') {
    isotopes <- read.csv(system.file("extdata",
                                     isotope_file,
                                     package = "isoH2Obudget",
                                     mustWork = TRUE))
  } else {
    isotopes <- read.csv(sprintf("%s/%s", filedir, isotope_file))
  }
  lake_isotopes <- subset(isotopes,
                          (Lake == lake | Site.ID == "PRECIP") &
                            Valid == TRUE &
                            is.na(d18O..VSMOW.) == FALSE,
                          select = c(Collection.Date.Time,
                                     Site.ID,
                                     d18O..VSMOW.,
                                     dD..VSMOW.))
  colnames(lake_isotopes) <- c("date", "site_id", "d18O", "d2H")
  lake_isotopes$date <- mdy_hm(lake_isotopes$date)

  return(as.data.frame(lake_isotopes))
}
