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
