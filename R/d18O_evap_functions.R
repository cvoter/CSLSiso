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
#' Calculates the isotope composition of evaporation.
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

d18O_evap <- function(atmp, ltmp, RH, d18O_pcpn, d18O_lake) {
  # Required parameters
  es_a          <- d18O_evap_sat_vapor_press(atmp)
  es_l          <- d18O_evap_sat_vapor_press(ltmp)
  h             <- d18O_evap_normalized_humidity(RH, es_a, es_l)
  alpha         <- d18O_evap_isotope_frac(ltmp)
  delta_epsilon <- d18O_evap_kinetic_frac(h, K = 14.3)
  epsilon       <- d18O_evap_total_frac(alpha, delta_epsilon)
  d18O_atm      <- d18O_evap_d18O_atm(d18O_pcpn, alpha)

  # d18O evaporation
  d18O_evap <- (alpha*d18O_lake - h*d18O_atm - epsilon)/
    (1 - h + delta_epsilon*10^(-3))
  return(d18O_evap)
}
# ------------------------------------------------------------------------------
#' Saturation Vapor Pressure
#'
#' Calculates the saturation vapor pressure based on temperature (of air or
#' water)
#'
#' @param tmp temperature of air or water (K)
#'
#' @return es - saturation vapor pressure
#'
#' @export

d18O_evap_sat_vapor_press <- function(tmp) {
  es <- (1321.7*exp(17.27*tmp/237 + tmp))/(273 + tmp)
  return(es)
}
# ------------------------------------------------------------------------------
#' Normalized Relative Humidity
#'
#' Calculates the relative humidity normalized to the temperature of the surface
#' water
#'
#' @param RH relative humidity (\%)
#' @param es_a saturation vapor pressure for the air
#' @param es_l saturation vapor pressure for the lake
#'
#' @return h - the relative humidity normalized to the temperature of the
#'         surface water
#'
#' @export

d18O_evap_normalized_humidity <- function(RH, es_a, es_l) {
  h <- RH * (es_a/es_l)
  return(h)
}
# ------------------------------------------------------------------------------
#' Equilibrium Isotope Fractionation Factor
#'
#' Calculates the equilibrium isotope fractionation factor at the temperature of
#' the air-water interface.
#'
#' @param ltmp lake surface temperature (K)
#'
#' @return alpha - the equilibrium isotope fractionation factor
#'
#' @export

d18O_evap_isotope_frac <- function(ltmp) {
  alpha <- (-2.0667 - (415.6/ltmp) + (1137/(ltmp^2))*(10^3))/1000
  return(alpha)
}
# ------------------------------------------------------------------------------
#' Kinetic Fractionation Factor
#'
#' Calculates the kinetic fractionation factor
#'
#' @param h relative humidity normalized to the temperature of the surface water
#' @param K constant determinded by wind tunnel experiments for different
#'          isotopes, defaults to K(18O) = 14.3.
#'
#' @return delta_epsilon - the kinetic fractionation factor
#'
#' @export

d18O_evap_kinetic_frac <- function(h, K = 14.3) {
  delta_epsilon <- K*(1-h)
  return(delta_epsilon)
}
# ------------------------------------------------------------------------------
#' Total Fractionation Factor
#'
#' Calculates the total fractionation factor
#'
#' @param alpha equilibrium isotope fractionation factor at the temperature of
#'              the air-water interface
#' @param delta_epsilon kinetic fractionation factor
#'
#' @return epsilon - the total fractionation factor
#'
#' @export

d18O_evap_total_frac <- function(alpha, delta_epsilon) {
  epsilon <- 1000*(1 - alpha) + delta_epsilon
}
# ------------------------------------------------------------------------------
#' Atmosphere d18O
#'
#' Calculates the d18O isotope composition of the atmosphere
#'
#' @param d18O_pcpn isotopic composition of precipitation
#' @param alpha equilibrium isotope fractionation factor at the temperature of
#'              the air-water interface
#'
#' @return d18O_atm - the d18O isotope composition of the atmosphere
#'
#' @export

d18O_evap_d18O_atm <- function(d18O_pcpn, alpha) {
  d18O_atm <- d18O_pcpn/alpha - (1 - 1/alpha)
  return(d18O_atm)
}
