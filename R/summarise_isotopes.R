#' Monthly Isotope Measurements
#'
#' Summarizes isotope measurements at a monthly timestep for a single lake
#'
#' @param isotopes a data frame with isotopes measurements as formatted in the
#'                 \code{\link{isotopes}} dataset, subset for a single lake.
#' @param site_dictionary a data frame with site id numbers and static isotope
#'                        site classifications as formatted in the
#'                        \code{\link{site_dictionary}} dataset, subset to
#'                        records for the lake of interest.
#' @param timeseries a vector of all months in the common timeseries among input
#'                   datasets
#' @param static_gw logical defaults to FALSE to use lake_levels and gw_levels
#'                  to define upgradient/downgradient wells at each measurement
#'                  date. If TRUE, uses static definitions of
#'                  upgradient/downgradient wells in site dictionary.
#' @param lake_levels a data frame with daily water level measurements as
#'                    formatted in the \code{\link{water_levels}} dataset,
#'                    subset to lake level records for the lake of interest.
#' @param gw_levels a data frame with daily water level measurements as
#'                  formatted in the \code{\link{water_levels}} dataset,
#'                  subset to groundwater level records at the lake of
#'                  interest.
#' @param median_threshold minimum median difference between lake levels and
#'                         groundwater levels during the month of measurement in
#'                         order to classify groundwater measurement.
#' @param static_lake logical defaults to FALSE to use actual measurement for
#'                    each month. If TRUE, uses mean of fall (Sept-Nov) isotope
#'                    samples for the lake.
#' @param use_kniffin_pcpn logical defaults to TRUE to average in precipitation
#'                         measurements by Maribeth Kniffin for each month.
#' @param pcpnfile name of file with dates of precipitation collector deployment.
#'                 Defaults to "csls_isotope_precipitation_deployment.csv"
#' @param pcpndir name of directory with file with dates of precipitation
#'                collector deployment. Defaults to "system.file" to indicate is
#'                stored within inst/extdata within the installed isoH2Obudget
#'                package files.
#'
#'
#' @return monthly_isotopes, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{d18O_pcpn}{mean precipitation stable isotope measurement for the month}
#' \item{d18O_lake}{mean lake stable isotope measurement for the month}
#' \item{d18O_GWin}{mean groundwater inflow stable isotope measurement for the
#'                  month}
#' \item{d18O_GWout}{mean groundwater outflow stable isotope measurement for the
#'                  month}
#' \item{GWin_sites}{well sites used in mean d18O_GWin each month}
#' \item{GWout_sites}{well sites used in mean d18O_GWout each month}
#' }
#'
#' @importFrom utils data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select group_by summarise ungroup
#' @importFrom reshape2 dcast
#' @importFrom rlang .data
#' @import lubridate
#' @importFrom stringr str_replace str_c
#'
#' @seealso \code{\link{retrieve_csls_isotopes}}, \code{\link{isotopes}},
#'          \code{\link{retrieve_csls_water_levels}},
#'          \code{\link{water_levels}},
#'          \code{\link{retrieve_csls_site_dictionary}},
#'          \code{\link{site_dictionary}}
#'
#' @export
summarise_isotopes <- function(isotopes, site_dictionary, timeseries,
                               static_gw = FALSE, lake_levels = NULL,
                               gw_levels = NULL, median_threshold = 0.01,
                               static_lake = FALSE, use_kniffin_pcpn = TRUE,
                               pcpnfile = 'csls_isotope_precipitation_deployment.csv',
                               pcpndir = "system.file") {
  # Assign site type to each measurement
  isotopes <- iso_site_type(isotopes, site_dictionary, static_gw,
                            lake_levels, gw_levels, median_threshold)

  # Mean d180 by month and site type, drop values w/out site type
  isomelt <- isotopes %>%
             group_by(date = floor_date(.data$date, unit = "month"),
                      site_type = .data$site_type) %>%
             filter(is.na(.data$site_type) == FALSE &
                      .data$site_type != "") %>%
             summarise(mean_d18O = mean(.data$d18O))

  # Reshape and rename columns
  monthly_isotopes           <- dcast(isomelt,
                                      date ~ site_type,
                                      value.var = "mean_d18O")
  colnames(monthly_isotopes) <- colnames(monthly_isotopes) %>%
                                str_replace("downgradient","d18O_GWout") %>%
                                str_replace("upgradient","d18O_GWin") %>%
                                str_replace("precipitation","d18O_pcpn") %>%
                                str_replace("lake","d18O_lake")
  if (!"d18O_GWout" %in% colnames(monthly_isotopes)) {
    monthly_isotopes$d18O_GWout <- NA
  }

  # Fill gaps in timeseries, values, and add notes on which gw well used
  monthly_isotopes <- monthly_isotopes %>%
                      filter(.data$date %in% timeseries)
  monthly_isotopes <- fill_timeseries_gaps(monthly_isotopes, timeseries)
  monthly_isotopes <- iso_lake_gapfill(monthly_isotopes)
  monthly_isotopes <- iso_pcpn_gapfill(monthly_isotopes, use_kniffin_pcpn,
                                       pcpnfile, pcpndir)
  monthly_isotopes <- iso_gw_site_names(monthly_isotopes, isotopes)

  return(as.data.frame(monthly_isotopes))
}
