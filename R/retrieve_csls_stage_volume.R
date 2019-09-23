#' Retrieve Central Sands Bathymetry Information
#'
#' Imports static bathymetry data for the CSLS lakes which was exported from
#' ArcGIS. Retains only stage (m), surface area (m^2), and volume (m^3).
#'
#' @param filename name of stage volume csv exported from ArcGIS
#' @param filedir name of directory with both csv files, defaults to
#'                'system.file' to instruct it to look within package
#'                "inst/extdata" directory.
#'
#' @return stage_vol, a data frame with the following columns:
#' \describe{
#' \item{lake}{lake associated with measurement}
#' \item{stage_m}{stage of lake (m) above mean sea level}
#' \item{surf_area_m2}{surface area of lake (m^2) at given elevation}
#' \item{volume_m3}{volume of lake (m^3) at given elevation}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select summarise group_by
#' @importFrom rlang .data
#' @importFrom NISTunits NISTftTOmeter NISTacreTOsqrMeter
#'
#' @export


retrieve_csls_stage_volume <- function(filename, filedir = 'system.file') {

  # Load csv with ArcGIS-derived stage-volume information
  stage_vol <- load_pkg_csv(filename, filedir)

  # Retain Lake, Depth, Elevation, Surface Area, and Volume (in m, m^2, m^3)
  stage_vol           <- stage_vol %>%
                         mutate(Depth_m = NISTftTOmeter(.data$Depth_ft),
                                Elevation_m = NISTftTOmeter(.data$Elevation_ft),
                                CumSA_m2 = NISTacreTOsqrMeter(.data$CumSA_ac)) %>%
                         select(.data$lake,
                                .data$Depth_m,
                                .data$Elevation_m,
                                .data$CumSA_m2,
                                .data$CumVol_m3)
  colnames(stage_vol) <- c("lake",
                           "depth_m",
                           "stage_m",
                           "surf_area_m2",
                           "volume_m3")

  # Add in zero depth info
  zero_elev              <- stage_vol %>%
                            group_by(lake = .data$lake) %>%
                            summarise(stage_m = max(.data$stage_m)+ min(.data$depth_m))
  zero_elev$depth_m      <- 0
  zero_elev$surf_area_m2 <- 0
  zero_elev$volume_m3    <- 0
  zero_elev              <- zero_elev %>%
                            select(.data$lake,
                                   .data$depth_m,
                                   .data$stage_m,
                                   .data$surf_area_m2,
                                   .data$volume_m3)
  stage_vol              <- rbind(stage_vol, zero_elev)

  # Flip vol and surf area so bottom of the lake = zero, top of lake = max
  stage_vol <- stage_vol %>%
               group_by(.data$lake) %>%
               mutate(surf_area_m2 = max(.data$surf_area_m2) - .data$surf_area_m2,
                      volume_m3 = max(.data$volume_m3) - .data$volume_m3)

  return(stage_vol)
}
