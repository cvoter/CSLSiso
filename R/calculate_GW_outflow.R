#' Groundwater Outflow
#'
#' Calculates the groundwater outflow as the remaining term in a water balance.
#'
#' @param P precipitation (mm)
#' @param E evapotranspiration (mm)
#' @param GWin groundwater inflow (mm)
#' @param dVdt change in lake volume storage (mm)
#'
#' @return GWout - the groundwtaer outflow from the lake (mm)
#'
#' @export

calculate_GW_outflow <- function(P, E, GWin, dVdt = 0) {
  GWout <- P + GWin - E - dVdt
  return(GWout)
}
