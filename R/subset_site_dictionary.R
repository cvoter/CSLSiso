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
