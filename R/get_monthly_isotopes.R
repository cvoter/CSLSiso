#' Get Monthly Isotopes
#'
#' This function summarizes isotope measurements for precipitation, the lake,
#' and inflowing groundwater wells at a monthly time step.
#'
#' @param isotope_file name of isotope csv file with:
#' \itemize {
#' \item
#' }
#' @param sites_file name of csv file with info on the sampling sites,
#'                   including:
#' \itemize {
#' \item
#' }
#' @param filedir name of directory with csv files, defaults to "data"
#'
#' @return monthly_isotopes, a data frame with the following:
#' \describe {
#' \item{}{}
#' }
#'
#' @import lubridate
#' @importFrom utils read.csv
#'
#' @export

get_monthly_isotopes <- function(isotope_file,
                                 sites_file,
                                 filedir = 'data') {
  # Load data
  sites <- read.csv(sprintf("%s/%s", filedir, sites_file))
  isotopes <- read.csv(sprintf("%s/%s", filedir, isotope_file))
  lake_isotopes <- subset(isotopes,
                          Site.ID %in% sites$site_id &
                            Valid == TRUE &
                            is.na(d18O..VSMOW.) == FALSE,
                          select = c(Collection.Date.Time,
                                     Site.ID,
                                     d18O..VSMOW.))
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
