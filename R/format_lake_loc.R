#' Format Lake Location Info for Lake Evaporation
#'
#' Extracts location information for the lake from the site dictionary and
#' reformats for input into lake evaporation functions.
#'
#' @param site_dictionary a data frame with the elev_m, lat_deg, and long_deg of
#'                        measurement sites, as in the
#'                        site_dictionary dataset, subset for a
#'                        single lake.
#' @param Lz the longitude of the local timezone (degrees west of Greenwich,
#'           ranges from 0 to 360 degrees). Defaults to 90 for Central Time
#'           Zone, USA.
#'
#' @return loc, a list with the following location-specific parameters:
#' \describe{
#' \item{z}{elevation above mean sea level (m)}
#' \item{phi}{latitude (radians). Positive for northern hemisphere, negative for
#'            southern hemisphere.}
#' \item{Lm}{longitude of location (degrees west of Greenwich)}
#' \item{Lz}{longitude of location's timezone (degrees west of Greenwich)}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @importFrom NISTunits NISTdegTOradian
#'
#' @export
format_lake_loc <- function(dictionary, Lz = 90) {
  loc     <- dictionary %>%
             filter(.data$obs_type == "LK") %>%
             select(z = .data$elev_m,
                    phi = .data$lat_deg,
                    Lm = .data$long_deg)
  loc$phi <- NISTdegTOradian(loc$phi)
  loc$Lm  <- loc$Lm
  loc$Lz  <- Lz
  loc     <- as.list(loc)
  return(loc)
}
