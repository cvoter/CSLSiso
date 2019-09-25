#' Monthly Weather
#'
#' Summarizes sub-monthly weather at a monthly timestep
#'
#' @param weather a data frame with sub-monthly weather including date, TMP (air
#'                temperature, deg C), RH (relative humidity, percent), P
#'                (precipitation, mm), and ET (evapotranspiration, mm) as
#'                formatted in the \code{\link{weather}} dataset.
#'
#' @return monthly_weather, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{atmp_K}{mean monthly air temperature (K)}
#' \item{RH_pct}{mean monthly relative humidity (percent)}
#' \item{P_mm}{monthly precipitaiton (mm)}
#' \item{ET_mm}{monthly reference potential evapotranspiration (mm)}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @importFrom lubridate floor_date
#' @importFrom NISTunits NISTdegCtOk
#'
#' @seealso \code{\link{retrieve_csls_weather}}, \code{\link{weather}}
#'
#' @export
get_monthly_weather <- function(weather){
  monthly_weather <- weather %>%
    group_by(date = floor_date(.data$date, unit = "month")) %>%
    summarise(atmp_K = NISTdegCtOk(mean(.data$atmp)),
              RH_pct = mean(.data$RH),
              P_mm = sum(.data$P),
              ET_mm = sum(.data$ET))
  return(monthly_weather)
}
