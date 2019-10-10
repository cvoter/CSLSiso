#' Subset Isotopes
#'
#' Subsets larger isotopes data frame to retain only precipitation measurements
#' plus lake and groundwater measurements associated with the lake of interest.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param isotopes a data frame with the date, lake, site_id, d18O, and d2H of
#'                 isotope measurements, as in the \code{\link{isotopes}}
#'                 dataset
#'
#' @return isotopes - a data frame with the same columns as in isotopes (see the
#'                    \code{\link{isotopes}} dataset), but subset for only
#'                    precipitation measurements plus lake and groundwater
#'                    measurements at the lake of interest.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @seealso \code{\link{retrieve_csls_isotopes}},
#'          \code{\link{isotopes}},
#'          \code{vignette("retrieve_csls_input_data", package = "isoH2Obudget")}
#'
#' @export
subset_isotopes <- function(lake, isotopes){
  isotopes <- isotopes %>%
              filter(.data$lake == !!lake | .data$site_id == "PRECIP")
  return(isotopes)
}
