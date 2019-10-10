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
summarise_lst <- function(lst, timeseries){
  monthly_lst <- lst %>%
                 group_by(date = floor_date(.data$date, unit = "month")) %>%
                 filter(date %in% timeseries) %>%
                 summarise(ltmp_K = NISTdegCtOk(mean(.data$ltmp)))
  monthly_lst <- fill_timeseries_gaps(monthly_lst, timeseries)
  return(monthly_lst)
}
