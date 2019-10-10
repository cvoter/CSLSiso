#d18O_evap_functions.R
# All functions required to calculate the isotopic composition of evaporation.
# Includes;
# - d18O_evap
# - d18O_evap_sat_vapor_press
# - d18O_evap_normalized_humidity
# - d18O_evap_isotope_frac
# - d18O_evap_kinetic_frac
# - d18O_evap_total_frac
# - d18O_evap_d18O_atm

# ------------------------------------------------------------------------------
#' Evaporation d18O
#'
#' Calculates the isotope composition of evaporation based on equation 5 in
#' Krabbenhoft et al. (1990). Note that alpha* is equivalent to 1/alpha.
#'
#' @references Krabbenhoft, D. P., C. J. Bowser, M. P. Anderson, and J. W.
#'   Valley. (1990). Estimating Groundwater Exchange with Lakes: 1. The Stable
#'   Isotope Mass Balance Method. Water Resources Research, 26(10):2445-2453.
#'   https://doi.org/10.1029/WR026i010p02445
#'
#' @param atmp air temperature (K)
#' @param ltmp lake surface temperature (K)
#' @param RH relative humidity (\%)
#' @param d18O_pcpn d18O isotopic composition of precipitation
#' @param d18O_lake d18O isotopic composition of the lake
#'
#' @return d18O_evap - the isotope composition of evaporation
#'
#' @export

calculate_d18O_evap <- function(atmp, ltmp, RH, d18O_pcpn, d18O_lake) {
  # Required parameters
  es_a          <- d18O_evap_sat_vapor_press(atmp - 273.15)
  es_l          <- d18O_evap_sat_vapor_press(ltmp - 273.15)
  h             <- d18O_evap_normalized_humidity(RH, es_a, es_l)
  alpha         <- d18O_evap_isotope_frac(ltmp)
  delta_epsilon <- d18O_evap_kinetic_frac(h, K = 14.3)
  epsilon       <- d18O_evap_total_frac(alpha, delta_epsilon)
  d18O_atm      <- d18O_evap_d18O_atm(d18O_pcpn, alpha)

  # d18O evaporation
  d18O_evap <- ((1/alpha)*d18O_lake - h*d18O_atm - epsilon)/
               (1 - h + delta_epsilon*10^(-3))
  return(d18O_evap)
}
# ------------------------------------------------------------------------------
#' Saturation Vapor Pressure
#'
#' Calculates the saturation vapor pressure based on temperature (of air or
#' water) based on equations 11 and 12 of Allen et al. (1998).
#'
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop
#'   evapotranspiration: Guidelines for computing crop water requirements. Rome:
#'   FAO. Retrieved from http://www.fao.org/docrep/X0490E/x0490e00.htm.
#'
#' @param tmp temperature of air or water (degrees C)
#'
#' @return es - saturation vapor pressure (kPa)
#'
#' @export

d18O_evap_sat_vapor_press <- function(tmp) {
  es <- 0.6108*exp(17.27*tmp/(237.3 + tmp))
  return(es)
}
# ------------------------------------------------------------------------------
#' Normalized Relative Humidity
#'
#' Calculates the relative humidity normalized to the temperature of the surface
#' water.
#'
#' @param RH relative humidity (\%)
#' @param es_a saturation vapor pressure for the air (kPa)
#' @param es_l saturation vapor pressure for the lake (kPa)
#'
#' @return h - the relative humidity normalized to the temperature of the
#'         surface water (-)
#'
#' @export

d18O_evap_normalized_humidity <- function(RH, es_a, es_l) {
  h <- (RH/100) * (es_a/es_l)
  return(h)
}
# ------------------------------------------------------------------------------
#' Equilibrium Isotope Fractionation Factor
#'
#' Calculates the equilibrium isotope fractionation factor at the temperature of
#' the air-water interface based on equation 9 in Ozaydin et al. (2001).
#'
#' @references Ozaydin, V., U. Sendil, and D. Altinbilek. (2001). Stable isotope
#'   mass balance method to find the water budget of a lake. Turkish Journal of
#'   Engineering and Environmental Sciences, 25:329-344. Retrieved from:
#'   https://dergipark.org.tr/download/article-file/126752
#'
#' @param ltmp lake surface temperature (K)
#'
#' @return alpha - the equilibrium isotope fractionation factor (-)
#'
#' @export

d18O_evap_isotope_frac <- function(ltmp){
  alpha <- exp(-7.685e-3 + 6.7123/(ltmp) - 1666.4/(ltmp)^2 + 350410/(ltmp)^3)
  return(alpha)
}
# ------------------------------------------------------------------------------
#' Kinetic Fractionation Factor
#'
#' Calculates the kinetic fractionation factor based on equation 6 in
#' Krabeenhoft et al. (1990).
#'
#' @references Krabbenhoft, D. P., C. J. Bowser, M. P. Anderson, and J. W.
#'   Valley. (1990). Estimating Groundwater Exchange with Lakes: 1. The Stable
#'   Isotope Mass Balance Method. Water Resources Research, 26(10):2445-2453.
#'   https://doi.org/10.1029/WR026i010p02445
#'
#'
#' @param h relative humidity normalized to the temperature of the surface water
#'          (-)
#' @param K constant determinded by wind tunnel experiments for different
#'          isotopes, defaults to K(18O) = 14.3.
#'
#' @return delta_epsilon - the kinetic fractionation factor (-)
#'
#' @export

d18O_evap_kinetic_frac <- function(h, K = 14.3) {
  delta_epsilon <- K*(1-h)
  return(delta_epsilon)
}
# ------------------------------------------------------------------------------
#' Total Fractionation Factor
#'
#' Calculates the total fractionation factor based on the definition of epsilon
#' for equation 5 in Krabbenhoft et al. (1990). Note that alpha* is equivalent
#' to 1/alpha.
#'
#' @references Krabbenhoft, D. P., C. J. Bowser, M. P. Anderson, and J. W.
#'   Valley. (1990). Estimating Groundwater Exchange with Lakes: 1. The Stable
#'   Isotope Mass Balance Method. Water Resources Research, 26(10):2445-2453.
#'   https://doi.org/10.1029/WR026i010p02445
#'
#' @param alpha equilibrium isotope fractionation factor at the temperature of
#'              the air-water interface (-)
#' @param delta_epsilon kinetic fractionation factor (-)
#'
#' @return epsilon - the total fractionation factor (-)
#'
#' @export

d18O_evap_total_frac <- function(alpha, delta_epsilon) {
  epsilon <- 1000*(1 - 1/alpha) + delta_epsilon
  return(epsilon)
}
# ------------------------------------------------------------------------------
#' Atmosphere d18O
#'
#' Calculates the d18O isotope composition of the atmosphere based on equation
#' 18 in Gibson et al. (2016).
#'
#' @references Gibson, J.J., S.J. Birks, and Y. Yi. 2016. Stable isotope mass
#'   balance of lakes: a contemporary perspective. Quaternary Science Reviews,
#'   131:316-328. https://doi.org/10.1016/j.quascirev.2015.04.013
#'
#' @param d18O_pcpn isotopic composition of precipitation
#' @param alpha isotope fractionation factor (-)
#' @param k weighted factor which reflects seasonality, ranging from 0.5 for
#'          highly seasonal climates to 1 for non-seasonal climates. Defaults
#'          to 0.8
#'
#' @return d18O_atm - the d18O isotope composition of the atmosphere
#'
#' @export

d18O_evap_d18O_atm <- function(d18O_pcpn, alpha, k = 1) {
  epsilon_plus     <- (alpha - 1)*1000
  d18O_atm         <- (d18O_pcpn - k*epsilon_plus)/(1 + k*1e-3*epsilon_plus)
  return(d18O_atm)
}
