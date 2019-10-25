#' Format Lake Size & Depth Info for Lake Evaporation
#'
#' Extracts surface area and depth of lake and re-formats for input to lake
#' evaporation functions.
#'
#' @inheritParams calculate_lake_evap
#'
#' @return lake, a list with lake data that includes:
#' \describe{
#' \item{A}{surface area of the lake (km^2).}
#' \item{depth_m}{depth of the lake (m)}
#' }
#'
#' @export
format_lake_info <- function(elev_area_vol, lst) {
  lake <- list(A = 1e-6*max(elev_area_vol$area_m2),
               depth_m = max(elev_area_vol$elev_m) - min(elev_area_vol$elev_m),
               lst = lst)
  return(lake)
}
