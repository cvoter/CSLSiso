#' Calculate Water Balance
#'
#' Give all required inputs, calculates the remaining terms of the water
#' balance: groundwater inflow and groundwater outflow.
#'
#' @param inputs a data frame with the date, air temperature (in degrees
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

calculate_h2o_bal <- function(inputs){
  # Groundwater inflow
  h2o_bal <- inputs %>%
             mutate(GWin_m3 = calculate_GW_inflow(.data$P_m3,
                                                .data$E_m3,
                                                .data$d18O_pcpn,
                                                .data$d18O_lake,
                                                .data$d18O_GWin,
                                                .data$d18O_evap,
                                                .data$mean_vol_m3,
                                                .data$delta_d18O_lake)) %>%
              mutate(GWin_mm = calculate_GW_inflow(.data$P_mm,
                                                   .data$E_mm,
                                                   .data$d18O_pcpn,
                                                   .data$d18O_lake,
                                                   .data$d18O_GWin,
                                                   .data$d18O_evap,
                                                   .data$mean_vol_m3/.data$mean_area_m2,
                                                   .data$delta_d18O_lake)) %>%
             select(.data$date, .data$P_m3, .data$E_m3, .data$dV_m3,
                    .data$GWin_m3, .data$P_mm, .data$E_mm, .data$dV_mm,
                    .data$GWin_mm, .data$mean_vol_m3, .data$mean_area_m2)
  # Groundwater outflow
  h2o_bal <- h2o_bal %>%
             mutate(GWout_m3 = calculate_GW_outflow(.data$P_m3,
                                                    .data$E_m3,
                                                    .data$GWin_m3,
                                                    .data$dV_m3),
                    GWout_mm = calculate_GW_outflow(.data$P_mm,
                                                    .data$E_mm,
                                                    .data$GWin_mm,
                                                    .data$dV_mm)) %>%
                    select(.data$date, .data$P_m3, .data$E_m3, .data$dV_m3,
                           .data$GWin_m3, .data$GWout_m3, .data$P_mm,
                           .data$E_mm, .data$dV_mm, .data$GWin_mm,
                           .data$GWout_mm, .data$mean_vol_m3,
                           .data$mean_area_m2)
  # As percents (in addition to mm and m3)
  h2o_bal <- h2o_bal %>%
             mutate(P_pcnt = .data$P_m3/(.data$GWin_m3 + .data$P_m3),
                    E_pcnt = .data$E_m3/(.data$GWin_m3 + .data$P_m3),
                    dV_pcnt = .data$dV_m3/(.data$GWin_m3 + .data$P_m3),
                    GWin_pcnt = .data$GWin_m3/(.data$GWin_m3 + .data$P_m3),
                    GWout_pcnt = .data$GWout_m3/(.data$GWin_m3 + .data$P_m3))

  return(h2o_bal)
}
