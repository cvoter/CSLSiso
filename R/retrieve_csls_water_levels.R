#' Retrieve Daily Central Sands Water Levels
#'
#' Downloads current feature layer from DNR feature services with water quantity
#' measurements in the Central Sands (summarized at a daily time step). Feature
#' layer of interest is DG_HiCap > DG_CSLS_QUANT_MON_WTM_EXT > Layer 2
#' (W13101.WU_CSLS_QUANT_DATA).
#'
#' @return water_levels, a data frame with the following columns
#' \describe{
#' \item{date}{date of water level measurement}
#' \item{site_no}{UGSG site number}
#' \item{obs_type}{type of observation (LK = lake level, GW = groundwater
#'                 level)}
#' \item{level_m}{water level in meters above mean sea level}
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select
#' @importFrom rlang .data
#'
#' @export

retrieve_csls_water_levels <- function() {
  levels_url  <- "https://uadnrmaps.wi.gov/arcgis/rest/services/DG_HiCap/DG_CSLS_QUANT_MON_WTM_EXT/FeatureServer/2/query?f=pjson&where=1=1&outfields=*"
  water_levels <- fromJSON(levels_url)
  water_levels <- as.data.frame(water_levels$features$attributes)
  water_levels <- water_levels %>%
                  mutate(date = as_datetime(.data$OBS_DATE/1000))
  water_levels <- water_levels %>%
                  filter(.data$OBS_TYPE %in% c("LK","GW")) %>%
                  select(.data$date, .data$SITE_NO, .data$OBS_TYPE, .data$OBS)

  colnames(water_levels) <- c("date", "site_no", "obs_type","level_m")

  return(water_levels)
}
