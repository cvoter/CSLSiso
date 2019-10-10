#' Calculate Water Balance
#'
#' Give all required inputs, calculates the remaining terms of the water
#' balance: groundwater inflow and groundwater outflow.
#'
#' @param h2o_bal_inputs a data frame with the date, air temperature (in degrees
#'                       C and Kelvin), relative humidity (percent),
#'                       precipitation (mm), lake evaporation (m), lake surface
#'                       temperature (Kelvin), change in lake volume (mm), d18O
#'                       isotopic composition of the lake, d18O isotopic
#'                       composition of precipitation, d18O isotopic composition
#'                       of GWin, d18O isotopic composition of GWout, d18O
#'                       isotopic composition of evaporation, and the well sites
#'                       used in each d18O_GWin and d18O_GWout mean. As output
#'                       by \code{\link{summarise_inputs}}.
#'
#' @return h2o_bal, the caculated water balance fluxes including precipitation,
#'         lake evaporation, change in lake volume, groundwater inflow, and
#'         groundwater outflow.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate
#' @importFrom rlang .data
#'
#' @seealso \code{\link{summarise_inputs}}
#'
#' @export

calculate_h2o_bal <- function (h2o_bal_inputs){
  h2o_bal <- h2o_bal_inputs %>%
              mutate(GWin = calculate_GW_inflow(.data$P_mm,
                                                .data$ET_mm,
                                                .data$d18O_pcpn,
                                                .data$d18O_lake,
                                                .data$d18O_GWin,
                                                .data$d18O_evap)) %>%
             select(.data$date, .data$P_mm, .data$ET_mm, .data$dV, .data$GWin)
  h2o_bal <- h2o_bal %>%
             mutate(GWout = calculate_GW_outflow(.data$P_mm,
                                                 .data$ET_mm,
                                                 .data$GWin,
                                                 .data$dV))
  return(h2o_bal)
}
