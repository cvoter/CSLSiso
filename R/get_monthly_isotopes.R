#' Get Monthly Isotopes
#'
#' This function summarizes isotope measurements for precipitation, the lake,
#' and inflowing groundwater wells at a monthly time step. Requires one csv file
#' with stable isotope measurements and a second csv that acts as a key to the
#' relevant site ids for the lake of interest and also indicates what type of
#' sites they are (i.e., precipitaiton, lake, upstream groundwater well, or
#' downstream groundwater well). Groundwater inflow wells are determined a
#' priori based on visual inspection of groundwater levels in each well, lake
#' water level, and stable isotope measurements.
#'
#' @param isotope_file name of isotope csv file with stable isotope measurement
#'                     info, including (but not limited to):
#' \itemize{
#' \item Site.ID - unique site id for where location was taken (e.g. LL-01 for
#'      Long Lake well 1). Assumed to match site ids in sites_file.
#' \item Valid - field indicating whether or not sample measurement is valid
#'       for analysis (TRUE) or not (FALSE).
#' \item d18O..VSMOW. - stable isotope measurement for d18O at this site
#' \item Collection.Date.Time - date and time of sample collection
#' }
#' @param sites_file name of csv file with info on the sampling sites for a
#'                   lake of interest, including:
#' \itemize{
#' \item site_id - unique site id for all stable isotope measurements relevant
#'       to this lake. Assumed to match site ids in isotope_file.
#' \item site_type - type of measurement location. Options include
#'       "precipitation", "lake", "upstream" (i.e., groundwater inflow well),
#'       "downstream" (i.e., groundwater outflow well), or "NA".
#' }
#' @param filedir name of directory with both csv files, defaults to
#'                'system.file' to instruct it to look within package
#'                "inst/extdata" directory.
#'
#' @return monthly_isotopes, a data frame with the following:
#' \describe{
#' \item{date}{first of the month for each monthly observation (Date)}
#' \item{d18O_pcpn}{mean precipitation stable isotope measurement for the month}
#' \item{d18O_lake}{mean lake stable isotope measurement for the month}
#' \item{d18O_GWin}{mean groundwater inflow stable isotope measurement for the
#'                  month}
#' }
#'
#' @import lubridate
#' @importFrom utils read.csv
#' @importFrom rlang .data
#'
#' @export

get_monthly_isotopes <- function(isotope_file,
                                 sites_file,
                                 filedir = 'system.file') {
  # Load data
  if (filedir == 'system.file') {
    sites <- read.csv(system.file("extdata",
                                  sites_file,
                                  package = "isoH2Obudget",
                                  mustWork = TRUE))
    isotopes <- read.csv(system.file("extdata",
                                     isotope_file,
                                     package = "isoH2Obudget",
                                     mustWork = TRUE))
  } else {
    sites <- read.csv(sprintf("%s/%s", filedir, sites_file))
    isotopes <- read.csv(sprintf("%s/%s", filedir, isotope_file))
  }
  lake_isotopes <- subset(isotopes,
                          .data$Site.ID %in% sites$site_id &
                            .data$Valid == TRUE &
                            is.na(.data$d18O..VSMOW.) == FALSE,
                          select = c(.data$Collection.Date.Time,
                                     .data$Site.ID,
                                     .data$d18O..VSMOW.))
  colnames(lake_isotopes) <- c("date","site_id","d18O")
  lake_isotopes <- merge(lake_isotopes, sites)

  # Identify start month and total number of months
  lake_isotopes$date <- as.Date(lake_isotopes$date, format = "%m/%d/%Y")
  month_info         <- start_n_months(lake_isotopes$date, all_days = FALSE)

  # Extract monthly isotopes
  monthly_isotopes <- NULL
  for (i in 1:month_info$nmonths) {
    this_month <- month_info$start_date + months(i-1)
    m          <- month(this_month)
    y          <- year(this_month)

    these_iso <- lake_isotopes[which(year(lake_isotopes$date) == y &
                                           month(lake_isotopes$date) == m),]
    d18O_pcpn <- these_iso$d18O[which(these_iso$site_type == "precipitation")]
    d18O_lake <- these_iso$d18O[which(these_iso$site_type == "lake")]
    d18O_GWin <- these_iso$d18O[which(these_iso$site_type == "upstream")]

    monthly_isotopes$date[i]      <- this_month
    monthly_isotopes$d18O_pcpn[i] <- mean(d18O_pcpn)
    monthly_isotopes$d18O_lake[i] <- mean(d18O_lake)
    monthly_isotopes$d18O_GWin[i] <- mean(d18O_GWin)
  }

  # R bizzarly looses the class of date objects in for loops, fix here
  monthly_isotopes$date <- as.Date(monthly_isotopes$date, origin = "1970-01-01")

  return(as.data.frame(monthly_isotopes))
}
