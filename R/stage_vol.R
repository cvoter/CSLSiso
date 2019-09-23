#' CSLS Bathymetry Information
#'
#' Bathymetry information about CSLS lakes including elevation, volume, and
#' surface area.
#'
#' Raw data are exported from ArcGIS polygons of elevation contours for each
#' lake.
#'
#' Raw csv data is processed with the function
#' \code{\link{retrieve_csls_stage_volume}} from the isoH2Obudget package. Lake
#' volume and surface area are defined such that they are a maximum at a depth
#' of 0 and drop to zero by the lowest elevation layer.
#'
#' @seealso \code{\link{retrieve_csls_stage_volume}}
#'
#' @docType data
#'
#' @usage data(stage_vol)
#'
#' @format A data frame with the following columns:
#' \describe{
#' \item{depth_m}{depth below surface of the lake}
#' \item{lake}{lake associated with measurement}
#' \item{stage_m}{stage of lake (m) above mean sea level}
#' \item{surf_area_m2}{surface area of lake (m^2) at given elevation}
#' \item{volume_m3}{volume of lake (m^3) at given elevation}
#' }
#'
"stage_vol"
