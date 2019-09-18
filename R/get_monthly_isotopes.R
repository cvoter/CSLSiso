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
#' @param lake name of lake to analyze (capitalized, e.g. "Pleasant")
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
#' @return monthly_isotopes, a data frame with the following:
#' \describe{
#' \item{date}{first of the month for each monthly observation (Date)}
#' \item{d18O_pcpn}{mean precipitation stable isotope measurement for the month}
#' \item{d18O_lake}{mean lake stable isotope measurement for the month}
#' \item{d18O_GWin}{mean groundwater inflow stable isotope measurement for the
#'                  month}
#' }
#'
#' @importFrom lubridate month year as_datetime
#'
#' @export

get_monthly_isotopes <- function(lake,
                                 isotope_file,
                                 site_file = "csls_site_dictionary.csv",
                                 filedir = 'system.file',
                                 Xday = 7) {

  # Extract stable isotope measurements for this lake
  isotopes     <- load_lake_isotopes(lake, isotope_file, filedir)

  # Retrieve water level information from DNR feature services
  water_levels <- retrieve_csls_water_levels()

  # Note which measurements are for upgradient vs. downgradient wells
  isotopes     <- classify_lake_isotopes(isotopes,
                                         water_levels,
                                         lake,
                                         site_file,
                                         filedir,
                                         Xday)

  # Identify start month and total number of months
  month_info <- start_n_months(isotopes$date, all_days = FALSE)

  # Extract monthly isotopes
  monthly_isotopes <- NULL
  for (i in 1:month_info$nmonths) {
    this_month <- month_info$start_date + months(i-1)
    m          <- month(this_month)
    y          <- year(this_month)

    these_iso <- isotopes[which(year(isotopes$date) == y &
                                  month(isotopes$date) == m),]
    d18O_pcpn <- these_iso$d18O[which(these_iso$site_type == "precipitation")]
    d18O_lake <- these_iso$d18O[which(these_iso$site_type == "lake")]
    d18O_GWin <- these_iso$d18O[which(these_iso$site_type == "upgradient")]

    monthly_isotopes$date[i]      <- this_month
    monthly_isotopes$d18O_pcpn[i] <- mean(d18O_pcpn)
    monthly_isotopes$d18O_lake[i] <- mean(d18O_lake)
    monthly_isotopes$d18O_GWin[i] <- mean(d18O_GWin)
  }

  # R bizzarly looses the class of date objects in for loops, fix here
  monthly_isotopes$date <- as_datetime(monthly_isotopes$date)

  return(as.data.frame(monthly_isotopes))
}
