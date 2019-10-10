#' Format Lake Size & Depth Info for Lake Evaporation
#'
#' Extracts surface area and depth of lake and re-formats for input to lake
#' evaporation functions.
#'
#' @param stage_vol a data frame with the lake, stage_m, surf_area_m2, and
#'                  volume_m3 as in the \code{\link{stage_vol}} dataset, subset
#'                  for a single lake.
#'
#' @return lake, a list with lake data that includes:
#' \describe{
#' \item{A}{surface area of the lake (km^2).}
#' \item{depth_m}{depth of the lake (m)}
#' }
#'
#' @seealso \code{\link{retrieve_csls_stage_vol}},
#'          \code{\link{stage_vol}}
#'
#' @export
format_lake_info <- function(stage_vol) {
  lake <- list(A = 1e-6*max(stage_vol$surf_area_m2),
               depth_m = max(stage_vol$depth_m))
  return(lake)
}
