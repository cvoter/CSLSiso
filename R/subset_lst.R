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
