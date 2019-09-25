#' Monthly Lake Surface Temperature
#'
#' Summarizes sub-monthly lake surface temperature at a monthly timestep for a
#' single lake
#'
#' @param lst a data frame with sub-monthly lake surface temperature
#'            measurements as formatted in the \code{\link{lst}} dataset, subset
#'            for a single lake.
#'
#' @return monthly_lst, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation}
#' \item{ltmp_K}{mean monthly lake surface temperature (degrees K)}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @importFrom lubridate floor_date
#' @importFrom NISTunits NISTdegCtOk
#'
#' @seealso \code{\link{retrieve_csls_lst}}, \code{\link{lst}}
#'
#' @export
get_monthly_lst <- function(lst){
  monthly_lst <- lst %>%
    group_by(date = floor_date(.data$date, unit = "month")) %>%
    summarise(ltmp_K = NISTdegCtOk(mean(.data$ltmp)))
  return(monthly_lst)
}
