#' Classify Stable Isotope Measurements
#'
#' This function determines whether wells were upgradient or downgradient of the
#' lake on stable isotope sample dates.
#'
#' @param isotopes a data frame with the following columns:
#' @itemize{
#' \item date - date and time of sample collection (Date)
#' \item site_id - unique site id for where location was taken
#' \item d18O - stable isotope measurement for d18O at this site
#' }
#' @param lake name of lake to analyze (e.g., Pleasant, Long, or Plainfield)
#' @param lake_file filename of tab-delimited text file from USGS gage station.
#' @param well_file filename ....
#'
#' @return
#'
#' @importFrom utils read.table
#'
#' @export

classify_lake_isotopes <- function(isotopes,
                                lake,
                                lake_file,
                                well_file,
                                Xday = 7) {

  # Lake Level - import measurements
  lake_levels <- read.table('inst/extdata/long_lake_level_usgs_2019_09_16.txt',
                            sep="\t",
                            header = TRUE)
  lake_levels <- lake_levels[-1,] # First line is weird, nix it
  lake_levels <- lake_levels[, c("datetime",
                                 "X231502_62615",
                                 "X231502_62615_cd")]

  # Lake level - fix names & classes of columns
  colnames(lake_levels)     <- c("date","lake_level_ft","qualification")
  lake_levels$date          <- ymd_hm(lake_levels$date)
  lake_levels$lake_level_ft <- as.numeric(as.character(lake_levels$lake_level_ft))

  # Well level - import measurements


  for (i in 1:length(isotopes)) {

    # X day mean lake level ahead of isotope sample
    lake_meas_i <- which.min(abs(lake_levels$date - isotopes$date[i]))
    lake_Xday   <- lake_levels$date[lake_meas_i] - days(Xday)
    lake_Xday_i <- which.min(abs(lake_levels$date - lake_Xday))
    lake_mean   <- mean(lake_levels$lake_level_ft[lake_Xday_i:lake_meas_i])

    # X day mean well level ahead of isotope sample
    well_meas_i <- which.min(abs(well_levels$date - isotopes$date[i]))
    well_Xday   <- well_levels$date[well_meas_i] - days(Xday)
    well_Xday_i <- which.min(abs(well_levels$date - well_Xday))
    well_mean   <- mean(well_levels$well_level_ft[well_Xday_i:well_meas_i])

    if (well_mean > lake_mean) {
      isotopes$site_type[i] <- "upgradient"
    } else {
      isotopes$site_type[i] <- "downgradient"
    }
  }

  #ID lake & precip site_type

  return(isotopes)
}
