#' Subset Inputs for Given Lake
#'
#' This function subsets input data for isoH2Obudget for just the lake of
#' interest.
#'
#' @param lake lake of interest (e.g., "Pleasant", "Long", or "Plainfield")
#' @param site_dictionary a data frame with the following columns:
#' \itemize{
#' \item lake - name of lake associated with the measurement site. Corresponds
#'              to lake name argument in this function.
#' \item obs_type - type of observation (LK = lake, GW = groundwater
#'                  monitoring well)
#' \item site_id - unique site id for measurement site (e.g., LL-01).
#'                 Corresponds to site_id in "isotopes" data frame.
#' \item SWIMS_station_id - SWIMS station id, if exists for this site.
#' \item USGS_id - USGS site number. Corresponds to site_no in "water_levels"
#'                 data frame.
#' \item WBIC - water body identification code (WBIC), for lake sites only.
#' }
#' @param lst a data frame with the following columns:
#' \itemize{
#' \item WBIC - Water Body Identification Code for lake
#' \item date - date and time of observation
#' \item ltmp - lake surface temperature (degrees C)
#' \item units - units of lake surface temperature, should all be "DEGREES C"
#' }
#' @param isotopes a data frame with the following columns:
#' \itemize{
#'   \item date - date of measurement
#'   \item lake - lake associated with measurement
#'   \item site_id - unique ID for site of measurement, e.g. "PRECIP", "LONG",
#'                  "LL-01"
#'   \item d18O - isotopic composition for 18O (per mil)
#'   \item d2H - isotopic composition for duterium (per mil)
#' }
#' @param water_levels a data frame with the following columns:
#' \itemize{
#' \item date - date of water level measurement
#' \item site_no - UGSG site number
#' \item obs_type - type of observation (LK = lake level, GW = groundwater
#'                 level)
#' \item level_m - water level in meters above mean sea level
#' }
#' @param stage_vol a data frame with the following columns:
#' \itemize{
#' \item lake - lake associated with measurement
#' \item stage_m - stage of lake (m) above mean sea level
#' \item surf_area_m2 - surface area of lake (m^2) at given elevation
#' \item volume_m3 - volume of lake (m^3) at given elevation
#' }
#'
#' @return a list with the input data frames subsetted to measurements
#'   associated with the lake of interest. See details on input parameters for
#'   more on the structure of these data frames.
#' \describe{
#' \item{lst}{data frame with lake surface temperatures (deg C)}
#' \item{isotopes}{data frame with lake isotope measurements, including
#'                 precipitation measurements}
#' \item{lake_levels}{data frame with lake levels (m)}
#' \item{gw_levels}{data frame with groundwater levels (m) around the lake}
#' \item{stage_vol}{data frame with relationships among stage (m), volume (m^3),
#'                  and surface area (m^2) for the lake}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#' @importFrom bit64 as.integer64
#'
#' @export

subset_lake <- function(lake,
                        site_dictionary,
                        lst,
                        isotopes,
                        water_levels,
                        stage_vol) {
  # Get lake id info
  WBIC     <- site_dictionary %>%
              filter(.data$lake == !!lake,
                     .data$obs_type == "LK") %>%
              select(.data$WBIC) %>%
              as.integer()
  USGS_id  <- site_dictionary %>%
              filter(.data$lake == !!lake,
                     .data$obs_type == "LK") %>%
              select(.data$USGS_id) %>%
              as.numeric() %>%
              as.integer64()
  gw_sites <- site_dictionary %>%
              filter(.data$lake == !!lake,
                     .data$obs_type == "GW")

  # Subset input data to lake of interest
  lst              <- lst %>%
                      filter(.data$WBIC == !!WBIC)

  isotopes         <- isotopes %>%
                      filter(.data$lake == !!lake | .data$site_id == "PRECIP")

  lake_levels     <- water_levels %>%
                     filter(.data$site_no == USGS_id,
                            .data$obs_type == "LK")
  gw_levels       <- water_levels %>%
                     filter(.data$site_no %in% c(gw_sites$USGS_id),
                            .data$obs_type == "GW")

  stage_vol       <- stage_vol %>%
                     filter(.data$lake == !!lake)

  site_dictionary <- site_dictionary %>%
                     filter(.data$lake == !!lake)

  return(list(lst = lst,
              isotopes = isotopes,
              lake_levels = lake_levels,
              gw_levels = gw_levels,
              stage_vol = stage_vol,
              site_dictionary = site_dictionary))
}
