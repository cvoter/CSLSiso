#' Change in Lake Volume
#'
#' Calculates the change in lake volume over the course of the inputed time
#' series of daily lake stage measurements (m).
#'
#' @param stage_vol, a data frame with the following columns:
#' \itemize{
#' \item stage_m - stage of lake (m) above mean sea level
#' \item surf_area_m2 - surface area of lake (m^2) at given elevation
#' \item volume_m3 - volume of lake (m^3) at given elevation
#' }
#' @param daily_stage a data frame with the following columns:
#' \itemize{
#' \item date - date of lake stage measurement
#' \item stage_m - daily measurement of lake level (m)
#' }
#' @param as_depth defaults to TRUE to normalize volume (m^3) by average surface
#'                 area of lake during the time segment (m^2) and return change
#'                 in volume as a depth (mm). If FALSE, returns change in lake
#'                 volume as a volume (m^3).
#' @param tmp_depth_fix defaults to FALSE. If TRUE, calculates change in volume
#'                      as a depth based solely on change in stage (mm).
#'
#' @return dV, the change in lake volume during this time period as a volume
#'         (m^3) or depth (mm)
#'
#' @importFrom stats approxfun
#'
#' @export

calculate_dV <- function(stage_vol, daily_stage = NULL, as_depth = TRUE,
                         tmp_depth_fix = FALSE) {
  # ID first and last measurement of month
  stage_start <- daily_stage$stage_m[daily_stage$date == min(daily_stage$date)]
  stage_end   <- daily_stage$stage_m[daily_stage$date == max(daily_stage$date)]

  # Create interpolation function for stage-vol
  f_stage_vol <- approxfun(x = stage_vol$stage_m, y = stage_vol$volume_m3)

  # Calculate change in volume
  vol_start   <- f_stage_vol(stage_start)
  vol_end     <- f_stage_vol(stage_end)
  dV          <- vol_end - vol_start

  # If change in volume as depth (mm) is desired, normalize by mean surface area
  if (as_depth){
    f_stage_area    <- approxfun(x = stage_vol$stage_m, y = stage_vol$surf_area_m2)
    daily_surf_area <- f_stage_area(daily_stage$stage_m)
    dV              <- dV/mean(daily_surf_area) # m^3 to m
    dV              <- dV*1000 # m to mm
  }

  if(tmp_depth_fix) {
    dV <- (stage_end - stage_start)*1000
  }

  return(dV)
}
