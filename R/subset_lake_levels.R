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
