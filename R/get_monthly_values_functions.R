# get_monthly_values_functions.R
# Functions to summarize input data at a monthly timestep.
# Includes:
# - get_monthly_weather
# - get_monthly_lst
# - get_monthly_isotopes
# - get_monthly_d18O_evap
# - get_monthly_dV

# ------------------------------------------------------------------------------
#' Monthly Weather
#'
#' Summarizes sub-monthly weather at a monthly timestep
#'
#' @param weather a data frame with sub-monthly weather as formatted in the
#'                \code{\link{weather}} dataset.
#'
#' @return monthly_weather, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{atmp_K}{mean monthly air temperature (K)}
#' \item{relh_pct}{mean monthly relative humidity (percent)}
#' \item{pcpn_mm}{monthly precipitaiton (mm)}
#' \item{rpet_mm}{monthly reference potential evapotranspiration (mm)}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @importFrom lubridate floor_date
#' @importFrom NISTunits NISTdegCtOk
#'
#' @seealso \code{\link{retrieve_csls_weather}}, \code{\link{weather}}
#'
#' @export
get_monthly_weather <- function(weather){
    monthly_weather <- weather %>%
                       group_by(date = floor_date(.data$date, unit = "month")) %>%
                       summarise(atmp_K = NISTdegCtOk(mean(.data$atmp)),
                                 relh_pct = mean(.data$relh),
                                 pcpn_mm = sum(.data$pcpn),
                                 rpet_mm = sum(.data$rpet))
  return(monthly_weather)
}

# ------------------------------------------------------------------------------
#' Monthly Lake Surface Temperature
#'
#' Summarizes sub-monthly lake surface temperature at a monthly timestep for a
#' single lake
#'
#' @param lst a data frame with sub-monthly lake surface temperature
#'            measurements as formatted in the \code{\link{lst}} dataset, subset
#'            for a single lake.
#'
#' @return monthly_lst, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{ltmp_K}{mean monthly lake surface temperature (degrees K)}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @importFrom lubridate floor_date
#' @importFrom NISTunits NISTdegCtOk
#'
#' @seealso \code{\link{retrieve_csls_lst}}, \code{\link{lst}}
#'
#' @export
get_monthly_lst <- function(lst){
    monthly_lst <- lst %>%
                   group_by(date = floor_date(.data$date, unit = "month")) %>%
                   summarise(ltmp_K = NISTdegCtOk(mean(.data$ltmp)))
  return(monthly_lst)
}

# ------------------------------------------------------------------------------
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
#' @param Xday number of days to consider when calculating average water level
#'             before isotope measurements, defaults to 7 days.
#' @param static_lake logical defaults to TRUE to use mean of fall (Sept-Nov)
#'                     isotope samples for the lake. If FALSE, uses actual
#'                     measurement for each month.
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
                                 Xday = 7,
                                 static_lake = TRUE) {
  # Classify isotope measurements
  if (static_wells) {
    isotopes     <- classify_iso_site_static(isotopes, site_dictionary)
  } else {
    isotopes     <- classify_iso_site_dynamic(isotopes, lake_levels, gw_levels,
                                              site_dictionary, Xday = 7)
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
    next_month <- monthly_isotopes$date[i] + months(1)
    next_i     <- which(monthly_isotopes$date == next_month)
    if ((is.na(monthly_isotopes$d18O_pcpn[i])) &
        (length(next_i) > 0)) {
      monthly_isotopes$d18O_pcpn[i] <- monthly_isotopes$d18O_pcpn[next_i]
    }
  }

  # Lake site type: if static, use only fall measurement
  if (static_lake) {
    fall_d18O_lake <- monthly_isotopes %>%
                      filter(month(.data$date) %in% c(9,10,11)) %>%
                      select(.data$d18O_lake) %>%
                      unlist()
    mean_d18O_lake <- mean(fall_d18O_lake, na.rm = TRUE)
    if (is.na(mean_d18O_lake) == FALSE) {
      monthly_isotopes$d18O_lake <- mean_d18O_lake
    }
  }

  return(as.data.frame(monthly_isotopes))
}

# ------------------------------------------------------------------------------
#' Monthly Evaporation d18O
#'
#' Calculates monthly Evaporation d18O based on monthly weather, lake surface
#' temperature, and isotope measurements.
#'
#' @param monthly_weather a data frame as output by
#'                        \code{\link{get_monthly_weather}}
#' @param monthly_lst a data frame as output by \code{\link{get_monthly_lst}}
#' @param monthly_isotopes a data frame as output by
#'                         \code{\link{get_monthly_isotopes}}
#'
#' @return monthly_isotopes with d18O_evap column added
#'
#' @export

get_monthly_d18O_evap <- function(monthly_weather,
                                  monthly_lst,
                                  monthly_isotopes) {
  for (i in 1:nrow(monthly_isotopes)) {
    this_month <- monthly_isotopes$date[i]

    atmp       <- monthly_weather$atmp_K[monthly_weather$date == this_month]
    ltmp       <- monthly_lst$ltmp_K[monthly_lst$date == this_month]
    RH         <- monthly_weather$relh_pct[monthly_weather$date == this_month]
    d18O_pcpn  <- monthly_isotopes$d18O_pcpn[i]
    d18O_lake  <- monthly_isotopes$d18O_lake[i]

    if (any(is.na(c(atmp, ltmp, RH, d18O_pcpn, d18O_lake))) == FALSE &
        length(atmp) > 0 & length(ltmp) > 0 & length(RH) > 0 &
        length(d18O_pcpn) > 0 & length(d18O_lake) > 0 ) {
      monthly_isotopes$d18O_evap[i] <- d18O_evap(atmp, ltmp, RH, d18O_pcpn, d18O_lake)
    } else {
      monthly_isotopes$d18O_evap[i] <- NA
    }
  }
  return(as.data.frame(monthly_isotopes))
}

# ------------------------------------------------------------------------------
#' Monthly Change in Lake Volume
#'
#' Calculates the monthly change in lake volume based on daily lake levels and a
#' stage-volume relationship from bathymetry analysis. .
#'
#' @param lake_levels a data frame with daily water level measurements as
#'                    formatted in the \code{\link{water_levels}} dataset,
#'                    subset to lake level records for the lake of interest.
#' @param stage_vol a data frame with stage-volume-surface area relationships as
#'                  formatted in the \code{\link{stage_vol}} dataset, subset for
#'                  a single lake.
#' @param month_info a list with with the start date, end date, and number of
#'                   months in the common timeseries of input data, as output by
#'                   \code{\link{get_overlap_months}}
#' @param as_depth defaults to TRUE to normalize volume (m^3) by average surface
#'                 area of lake during the time segment (m^2) and return change
#'                 in volume as a depth (mm). If FALSE, returns change in lake
#'                 volume as a volume (m^3).
#' @param tmp_depth_fix defaults to FALSE. If TRUE, calculates change in volume
#'                      as a depth based solely on change in stage (mm).
#'
#' @return monthly_dV, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{dV}{change in lake volume during the month as a volume (m^3) or as a
#'           depth (mm), depending on value of as_depth}
#' }
#'
#' @import lubridate
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate
#' @importFrom rlang .data
#'
#' @export

get_monthly_dV <- function(lake_levels,
                           stage_vol,
                           month_info,
                           as_depth = TRUE,
                           tmp_depth_fix = FALSE) {
  monthly_dV <- NULL
  for (i in 1:month_info$nmonths) {
    this_month  <- month_info$start_date + months(i-1)
    daily_stage <- lake_levels %>%
                   filter(month(.data$date) == month(this_month),
                          year(.data$date) == year(this_month)) %>%
                   mutate(stage_m = .data$level_m) %>%
                   select(.data$date, .data$stage_m)
    dV          <- calculate_dV(stage_vol, daily_stage, as_depth, tmp_depth_fix)

    monthly_dV$date[i] <- this_month
    monthly_dV$dV[i]   <- dV
  }

  # R bizzarly looses the class of date objects in for loops, fix here
  monthly_dV$date <- as_datetime(monthly_dV$date)

  return(as.data.frame(monthly_dV))
}

# ------------------------------------------------------------------------------
#' Monthly Water Balance
#'
#' Calculates the water balance of a lake based measured fluxes and isotopic
#' signatures of precipitation, the lake, and inflowing groundwater wells. Based
#' on equations 2 and 4 in Krabbenhoft et al. (1990).
#'
#' @references Krabbenhoft, D. P., C. J. Bowser, M. P. Anderson, and J. W.
#'   Valley. (1990). Estimating Groundwater Exchange with Lakes: 1. The Stable
#'   Isotope Mass Balance Method. Water Resources Research, 26(10):2445-2453.
#'   https://doi.org/10.1029/WR026i010p02445
#'
#' @param monthly_weather a data frame as output by
#'                        \code{\link{get_monthly_weather}}
#' @param monthly_isotopes a data frame as output by
#'                         \code{\link{get_monthly_isotopes}}
#' @param monthly_dV a data frame as output by
#'                         \code{\link{get_monthly_dV}}
#' @param month_info a list with with the start date, end date, and number of
#'                   months in the common timeseries of input data, as output by
#'                   \code{\link{get_overlap_months}}
#'
#' @return monthly_h2o_bal, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation (Date)}
#' \item{P}{monthly precipitation (mm)}
#' \item{E}{monthly evapotranspiration (mm)}
#' \item{GWin}{monthly groundwater inflow to the lake (mm)}
#' \item{GWout}{monthly groundwater outflow to the lake (mm)}
#' \item{dV}{monthly change in lake volume (mm)}
#' }
#'
#' @import lubridate
#'
#' @export

get_monthly_h2o_bal <- function(monthly_weather,
                                monthly_isotopes,
                                monthly_dV,
                                month_info) {
  monthly_h2o_bal <- NULL
  for (i in 1:month_info$nmonths) {
    this_month <- month_info$start_date + months(i - 1)

    # Required inputs
    P          <- monthly_weather$pcpn_mm[monthly_weather$date == this_month]
    E          <- monthly_weather$rpet_mm[monthly_weather$date == this_month]
    dV         <- monthly_dV$dV[monthly_dV$date == this_month]
    d18O_pcpn  <- monthly_isotopes$d18O_pcpn[monthly_isotopes$date == this_month]
    d18O_lake  <- monthly_isotopes$d18O_lake[monthly_isotopes$date == this_month]
    d18O_GWin  <- monthly_isotopes$d18O_GWin[monthly_isotopes$date == this_month]
    d18O_evap  <- monthly_isotopes$d18O_evap[monthly_isotopes$date == this_month]

    # Monthly lake water balance
    monthly_h2o_bal$date[i] <- this_month
    monthly_h2o_bal$P[i]    <- P
    monthly_h2o_bal$E[i]    <- E
    monthly_h2o_bal$GWin[i] <- calculate_GW_inflow(P,
                                                   E,
                                                   d18O_pcpn,
                                                   d18O_lake,
                                                   d18O_GWin,
                                                   d18O_evap)
    monthly_h2o_bal$GWout[i] <- calculate_GW_outflow(P,
                                                     E,
                                                     monthly_h2o_bal$GWin[i],
                                                     dV)
    monthly_h2o_bal$dV[i]   <- dV
  }
  # R bizzarly looses the class of date objects in for loops, fix here
  # Format of date number requires lubridate::as_datetime, but convert from
  # POSIXct to Date for ggplotting
  monthly_h2o_bal$date <- as_datetime(monthly_h2o_bal$date)
  monthly_h2o_bal$date <- as.Date(monthly_h2o_bal$date)

  return(as.data.frame(monthly_h2o_bal))
}
