# subset_lake_functions.R
# Functions to subset larger input datasets into just records related to the
# lake of interest.
# Includes:
# - subset_lake
# - subset_lst
# - subset_isotopes
# - subset_gw_levels
# - subset_lake_levels
# - subset_stage_vol
# - subset_site_dictionary
#
# ------------------------------------------------------------------------------
#' Subset Inputs for Given Lake
#'
#' This function subsets input data for isoH2Obudget for just the lake of
#' interest. Includes: site_dictionary, lst, isotopes, water_levels (to
#' gw_levels and lake_levels), and stage_vol.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param site_dictionary a data frame with the lake, obs_type, site_id,
#'                        SWIMS_station, USGS_id, WBIC, and static_iso_type of
#'                        measurement sites, as in the
#'                        \code{\link{site_dictionary}} dataset
#' @param lst a data frame with the WBIC, date, ltmp, and units of
#'            every-other-week lake surface temperature measurements, as in the
#'            \code{\link{lst}} dataset
#' @param isotopes a data frame with the date, lake, site_id, d18O, and d2H of
#'                 isotope measurements, as in the \code{\link{isotopes}}
#'                 dataset
#' @param water_levels a data frame with the date, site_no, obs_type, and
#'                     level_m of daily water level observations, as in the
#'                     \code{\link{water_levels}} dataset.
#' @param stage_vol a data frame with the lake, stage_m, surf_area_m2, and
#'                  volume_m3 as in the \code{\link{stage_vol}} dataset.
#'
#' @return a list with the input data frames subsetted to measurements
#'   associated with the lake of interest. Includes:
#' \describe{
#' \item{lst}{data frame with lake surface temperatures (deg C)}
#' \item{isotopes}{data frame with lake isotope measurements, including
#'                 precipitation measurements}
#' \item{lake_levels}{data frame with lake levels (m)}
#' \item{gw_levels}{data frame with groundwater levels (m) around the lake}
#' \item{stage_vol}{data frame with relationships among stage (m), volume (m^3),
#'                  and surface area (m^2) for the lake}
#' \item{site_dictionary}{data frame with all names, identification codes, and
#'                        classifications of measurment sites associated with
#'                        the lake}
#' }
#'
#' @export

subset_lake <- function(lake,
                        site_dictionary,
                        lst,
                        isotopes,
                        water_levels,
                        stage_vol) {

  lst             <- subset_lst(lake, lst, site_dictionary)
  isotopes        <- subset_isotopes(lake, isotopes)
  lake_levels     <- subset_lake_levels(lake, water_levels, site_dictionary)
  gw_levels       <- subset_gw_levels(lake, water_levels, site_dictionary)
  stage_vol       <- subset_stage_vol(lake, stage_vol)
  site_dictionary <- subset_site_dictionary(lake, site_dictionary)

  return(list(lst = lst,
              isotopes = isotopes,
              lake_levels = lake_levels,
              gw_levels = gw_levels,
              stage_vol = stage_vol,
              site_dictionary = site_dictionary))
}

# ------------------------------------------------------------------------------
#' Subset Lake Surface Temperature
#'
#' Subsets larger lake surface temperature data frame to retain only those
#' measurements associated with the lake of interest.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param lst a data frame with the WBIC, date, ltmp, and units of
#'            every-other-week lake surface temperature measurements, as in the
#'            \code{\link{lst}} dataset
#' @param site_dictionary a data frame with the lake, obs_type, site_id,
#'                        SWIMS_station, USGS_id, WBIC, and static_iso_type of
#'                        measurement sites, as in the
#'                        \code{\link{site_dictionary}} dataset.
#'
#' @return lst - a data frame with the same columns as in lst (see the
#'               \code{\link{lst}} dataset), but subset for only lake surface
#'               temperatures atthe lake of interest.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#'
#' @seealso \code{\link{retrieve_csls_lst}},
#'          \code{\link{lst}}, \code{\link{site_dictionary}},
#'          \code{vignette("retrieve_csls_input_data", package = "isoH2Obudget")}
#'
#' @export
subset_lst <- function(lake, lst, site_dictionary){
  WBIC <- site_dictionary %>%
          filter(.data$lake == !!lake,
                 .data$obs_type == "LK") %>%
          select(.data$WBIC) %>%
          as.integer()
  lst  <- lst %>%
          filter(.data$WBIC == !!WBIC)
  return(lst)
}

# ------------------------------------------------------------------------------
#' Subset Isotopes
#'
#' Subsets larger isotopes data frame to retain only precipitation measurements
#' plus lake and groundwater measurements associated with the lake of interest.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param isotopes a data frame with the date, lake, site_id, d18O, and d2H of
#'                 isotope measurements, as in the \code{\link{isotopes}}
#'                 dataset
#'
#' @return isotopes - a data frame with the same columns as in isotopes (see the
#'                    \code{\link{isotopes}} dataset), but subset for only
#'                    precipitation measurements plus lake and groundwater
#'                    measurements at the lake of interest.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @seealso \code{\link{retrieve_csls_isotopes}},
#'          \code{\link{isotopes}},
#'          \code{vignette("retrieve_csls_input_data", package = "isoH2Obudget")}
#'
#' @export
subset_isotopes <- function(lake, isotopes){
  isotopes <- isotopes %>%
              filter(.data$lake == !!lake | .data$site_id == "PRECIP")
  return(isotopes)
}

# ------------------------------------------------------------------------------
#' Subset Groundwater Levels
#'
#' Subsets larger water level data frame by USGS_id to retrieve only groundwater
#' records for a given lake.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param water_levels a data frame with the date, site_no, obs_type, and
#'                     level_m of daily water level observations, as in the
#'                     \code{\link{water_levels}} dataset.
#' @param site_dictionary a data frame with the lake, obs_type, site_id,
#'                        SWIMS_station, USGS_id, WBIC, and static_iso_type of
#'                        measurement sites, as in the
#'                        \code{\link{site_dictionary}} dataset.
#'
#' @return gw_levels - a data frame with the same columns as in water_levels
#'                     (see the \code{\link{water_levels}} dataset), but subset
#'                     for only groundwater wells at the lake of interest. The
#'                     site_id (e.g., LL-01) is also attached.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @seealso \code{\link{retrieve_csls_water_levels}},
#'          \code{\link{water_levels}}, \code{\link{site_dictionary}},
#'          \code{vignette("retrieve_csls_input_data", package = "isoH2Obudget")}
#'
#' @export
subset_gw_levels <- function(lake, water_levels, site_dictionary){
  # Filter water levels to just lake groundwater sites
  gw_sites  <- site_dictionary %>%
               filter(.data$lake == !!lake,
                      .data$obs_type == "GW")
  gw_levels <- water_levels %>%
               filter(.data$site_no %in% c(gw_sites$USGS_id),
                      .data$obs_type == "GW")
  # Attach site id
  for (i in 1:nrow(gw_levels)) {
    site_no <- gw_levels$site_no[i]
    site_id <- as.character(gw_sites$site_id[gw_sites$USGS_id == site_no])

    gw_levels$site_id[i] <- site_id
  }
  return(gw_levels)
}

# ------------------------------------------------------------------------------
#' Subset Lake Levels
#'
#' Subsets larger water level data frame by USGS_id to retrieve only lake level
#' records for a given lake.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param water_levels a data frame with the date, site_no, obs_type, and
#'                     level_m of daily water level observations, as in the
#'                     \code{\link{water_levels}} dataset.
#' @param site_dictionary a data frame with the lake, obs_type, site_id,
#'                        SWIMS_station, USGS_id, WBIC, and static_iso_type of
#'                        measurement sites, as in the
#'                        \code{\link{site_dictionary}} dataset.
#'
#' @return lake_levels - a data frame with the same columns as in water_levels
#'                       (see the \code{\link{water_levels}} dataset), but
#'                       subset for only lake levels at the lake of interest.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom bit64 as.integer64
#' @importFrom rlang .data
#'
#' @seealso \code{\link{retrieve_csls_water_levels}},
#'          \code{\link{water_levels}}, \code{\link{site_dictionary}},
#'          \code{vignette("retrieve_csls_input_data", package = "isoH2Obudget")}
#'
#' @export
subset_lake_levels <- function(lake, water_levels, site_dictionary){
  USGS_id     <- site_dictionary %>%
                 filter(.data$lake == !!lake,
                        .data$obs_type == "LK") %>%
                 select(.data$USGS_id) %>%
                 as.numeric() %>%
                 as.integer64()
  lake_levels <- water_levels %>%
                 filter(.data$site_no == USGS_id,
                        .data$obs_type == "LK")
  return(lake_levels)
}

# ------------------------------------------------------------------------------
#' Subset Stage Volume Relationship
#'
#' Subsets larger stage_vol data frame to retain only those records related to
#' the lake of interest.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param stage_vol a data frame with the lake, stage_m, surf_area_m2, and
#'                  volume_m3 as in the \code{\link{stage_vol}} dataset.
#'
#' @return stage_vol - a data frame with the same columns as in stage_vol (see the
#'                    \code{\link{stage_vol}} dataset), but subset for only
#'                    those records for the lake of interest.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @seealso \code{\link{retrieve_csls_stage_volume}},
#'          \code{\link{stage_vol}},
#'          \code{vignette("retrieve_csls_input_data", package = "isoH2Obudget")}
#'
#' @export
subset_stage_vol <- function(lake, stage_vol){
  stage_vol <- stage_vol %>%
               filter(.data$lake == !!lake)
  return(stage_vol)
}

# ------------------------------------------------------------------------------
#' Subset Site Dictionary
#'
#' Subsets larger site_dictionary data frame to retain only those records
#' related to the lake of interest.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param site_dictionary a data frame with the lake, obs_type, site_id,
#'                        SWIMS_station, USGS_id, WBIC, and static_iso_type of
#'                        measurement sites, as in the
#'                        \code{\link{site_dictionary}} dataset
#'
#' @return site_dictionary - a data frame with the same columns as in
#'                           site_dictionary (see the
#'                           \code{\link{site_dictionary}} dataset), but subset
#'                           for only those records for the lake of interest.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @seealso \code{\link{retrieve_csls_site_dictionary}},
#'          \code{\link{site_dictionary}},
#'          \code{vignette("retrieve_csls_input_data", package = "isoH2Obudget")}
#'
#' @export
subset_site_dictionary <- function(lake, site_dictionary){
  site_dictionary <- site_dictionary %>%
                     filter(.data$lake == !!lake)
  return(site_dictionary)
}
