# GW_functions
# Functions to calculate groundwater flow to and from a lake
# Includes;
# - GW_inflow
# - GW_outflow
# ------------------------------------------------------------------------------
#' Groundwater Inflow
#'
#' Calculates the groundwater inflow to a lake based on equation 4 in
#' Krabbenhoft et al. (1990).
#'
#' @references Krabbenhoft, D. P., C. J. Bowser, M. P. Anderson, and J. W.
#'   Valley. (1990). Estimating Groundwater Exchange with Lakes: 1. The Stable
#'   Isotope Mass Balance Method. Water Resources Research, 26(10):2445-2453.
#'   https://doi.org/10.1029/WR026i010p02445
#'
#' @param P precipitation (mm)
#' @param E evapotranspiration (mm)
#' @param d18O_pcpn d18O isotopic composition of precipitation
#' @param d18O_lake d18O isotopic composition of the lake
#' @param d18O_GWin d18O isotopic composition of groundwater inflow
#' @param d18O_evap d18O isotopic composition of evaporation
#' @param V volume of water in the lake
#' @param d18O_lake_dt change in d18O_lake in over the timestep of interest
#'                     (e.g., 1 month)
#'
#' @return GWin - the groundwtaer inflow to the lake (mm)
#'
#' @export

calculate_GW_inflow <- function(P, E, d18O_pcpn, d18O_lake, d18O_GWin, d18O_evap,
                                V = 0, d18O_lake_dt = 0) {
  GWin <- (P*(d18O_lake - d18O_pcpn) + E*(d18O_evap - d18O_lake) + V*d18O_lake_dt)/
          (d18O_GWin - d18O_lake)

  if (length(GWin) == 0) {GWin <- NA}

  return(GWin)
}
# ------------------------------------------------------------------------------
#' Groundwater Inflow
#'
#' Calculates the groundwater inflow to a lake based on equation 4 in
#' Krabbenhoft et al. (1990).
#'
#' @references Krabbenhoft, D. P., C. J. Bowser, M. P. Anderson, and J. W.
#'   Valley. (1990). Estimating Groundwater Exchange with Lakes: 1. The Stable
#'   Isotope Mass Balance Method. Water Resources Research, 26(10):2445-2453.
#'   https://doi.org/10.1029/WR026i010p02445
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
