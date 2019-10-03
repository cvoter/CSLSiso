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
#' @param static_wells logical defaults to TRUE to use static definitions of
#'                     upgradient/downgradient wells in site dictionary. If
#'                     FALSE, uses lake_levels and gw_levels to define
#'                     upgradient/downgradient wells at each measurement date.
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
#' @param static_lake logical defaults to TRUE to use mean of fall (Sept-Nov)
#'                     isotope samples for the lake. If FALSE, uses actual
#'                     measurement for each month.
#' @param use_kniffin_pcpn logical defaults to TRUE to average in precipitation
#'                         measurements by Maribeth Kniffin for each month.
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
get_monthly_isotopes <- function(isotopes,
                                 site_dictionary,
                                 static_wells = TRUE,
                                 lake_levels = NULL,
                                 gw_levels = NULL,
                                 median_threshold = 0.01,
                                 static_lake = TRUE,
                                 use_kniffin_pcpn = TRUE) {
  # Classify isotope measurements
  if (static_wells) {
    isotopes     <- classify_iso_site_static(isotopes, site_dictionary)
  } else {
    isotopes     <- classify_iso_site_dynamic(isotopes, lake_levels, gw_levels,
                                              site_dictionary, median_threshold)
  }

  # Mean by month and site type
  isomelt <- isotopes %>%
             group_by(date = floor_date(.data$date, unit = "month"),
             site_type = .data$site_type) %>%
             filter(is.na(.data$site_type) == FALSE &
                      .data$site_type != "") %>%
             summarise(mean_d18O = mean(.data$d18O),
                       site_ids = str_c(unique(.data$site_id), collapse = ", "))

  # Reshape and rename columns
  monthly_isotopes           <- dcast(isomelt,
                                      date ~ site_type,
                                      value.var = "mean_d18O")
  colnames(monthly_isotopes) <- colnames(monthly_isotopes) %>%
                                str_replace("downgradient","d18O_GWout") %>%
                                str_replace("upgradient","d18O_GWin") %>%
                                str_replace("precipitation","d18O_pcpn") %>%
                                str_replace("lake","d18O_lake")

  monthly_isotopes$d18O_evap <- NA
  monthly_isotopes$GWin_sites <- ""
  monthly_isotopes$GWout_sites <- ""
  for (i in 1:nrow(monthly_isotopes)) {
    if (is.na(monthly_isotopes$d18O_GWin[i]) == FALSE) {
      monthly_isotopes$GWin_sites[i] <- isomelt %>%
                                        filter(.data$date == monthly_isotopes$date[i],
                                               .data$site_type == "upgradient") %>%
                                        ungroup() %>%
                                        select(.data$site_ids)
    }
    if (is.null(monthly_isotopes$d18O_GWout[i]) == FALSE) {
      if (is.na(monthly_isotopes$d18O_GWout[i]) == FALSE) {
        monthly_isotopes$GWout_sites[i] <- isomelt %>%
          filter(.data$date == monthly_isotopes$date[i],
                 .data$site_type == "downgradient") %>%
          ungroup() %>%
          select(.data$site_ids)
      }
    }
  }

  # Precipitation site type: if NA, use next month's value
  for (i in 1:nrow(monthly_isotopes)) {
    this_month <- monthly_isotopes$date[i]
    next_month <- this_month + months(1)
    next_i     <- which(monthly_isotopes$date == next_month)
    if ((is.na(monthly_isotopes$d18O_pcpn[i])) &
        (length(next_i) > 0)) {
      monthly_isotopes$d18O_pcpn[i] <- monthly_isotopes$d18O_pcpn[next_i]
    }
    if (use_kniffin_pcpn) {
      data(kniffin_isotopes, envir = environment())
      kniffin_pcpn <- kniffin_isotopes %>%
                      filter(month(.data$date) == month(this_month))
      monthly_isotopes$d18O_pcpn[i] <- mean(c(monthly_isotopes$d18O_pcpn[i],
                                              kniffin_pcpn$d18O_pcpn),
                                            na.rm = TRUE)
    }
  }

  # Lake site type: if static, use only fall measurement
  fall_d18O_lake <- monthly_isotopes %>%
    filter(month(.data$date) %in% c(9,10,11,12)) %>%
    select(.data$d18O_lake) %>%
    unlist()
  mean_d18O_lake <- mean(fall_d18O_lake, na.rm = TRUE)
  if (is.na(mean_d18O_lake) == FALSE) {
    if (static_lake) {
      monthly_isotopes$d18O_lake <- mean_d18O_lake
    } else {
      monthly_isotopes$d18O_lake[which(month(monthly_isotopes$date) %in% c(9,10,11,12))] <- mean_d18O_lake
    }
  }

  return(as.data.frame(monthly_isotopes))
}
