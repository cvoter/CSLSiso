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
