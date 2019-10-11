#' Run isoH2Obudget Start to Finish
#'
#' Runs all major functions in the isoH2Obudget package, from loading in raw
#' data to the final monthly water balance
#'
#' @param lake name of lake (e.g., Pleasant, Long, or Plainfield)
#' @param files a list that includes the following filenames:
#' \itemize{
#' \item weather_file
#' \item SWIMS_file
#' \item isotope_file
#' \item site_file
#' \item stage_vol_file
#' }
#' @param filedir name of directory with both csv files, defaults to
#'                'system.file' to instruct it to look within package
#'                "inst/extdata" directory
#' @param dataset dataset type to process. Expects "csls" or "kniffin".
#'
#' @return monthly_h2o_bal, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation (Date)}
#' \item{P}{monthly precipitation (mm)}
#' \item{E}{monthly evapotranspiration (mm)}
#' \item{GWin}{monthly groundwater inflow to the lake (mm)}
#' \item{GWout}{monthly groundwater outflow to the lake (mm)}
#' \item{dV}{monthly change in lake volume (mm)}
#' }
#'
#' @examples
#' \dontrun{
#' csls_files <- list(weather_file = "csls_weather_hancock_hourly_2019_09_30.csv",
#'                    SWIMS_file = "csls_SWIMS_2019_09_05.csv",
#'                    isotope_file = "csls_isotope_samples_2019_10_02.csv",
#'                    site_file = "csls_site_dictionary.csv",
#'                    stage_vol_file = "csls_stage_volume.csv")
#' lake = "Pleasant"
#' monthly_h2o_bal <- runall_isoH2Obudget(lake, csls_files,
#'                                        filedir = "system.file",
#'                                        dataset = "csls")
#'
#'
#' kniffin_files <- list(weather_file = "kniffin_weather_monthly.csv",
#'                       isotope_file = "kniffin_isotope_samples.csv",
#'                       site_file = "kniffin_site_dictionary.csv",
#'                       d18O_evap_file = "kniffin_d18O_evap.csv")
#' lake = "Long"
#' monthly_h2o_bal <- runall_isoH2Obudget(lake, kniffin_files,
#'                                        filedir = "system.file",
#'                                        dataset = "kniffin")
#' }
#' @importFrom usethis use_data
#' @importFrom NISTunits NISTinchTOmeter
#' @import lubridate
#'
#' @export

runall_isoH2Obudget <- function(lake, files, filedir = "system.file",
                                dataset = "csls"){
  if (dataset == "csls") {
    weather         <- retrieve_csls_weather(files$weather_file, filedir)
    lst             <- retrieve_csls_lst(files$SWIMS_file, filedir)
    isotopes        <- retrieve_csls_isotopes(files$isotope_file, filedir)
    water_levels    <- retrieve_csls_water_levels()
    stage_vol       <- retrieve_csls_stage_volume(files$stage_vol_file, filedir)
    site_dictionary <- retrieve_csls_site_dictionary(files$site_file, filedir)

    use_data(isotopes, lst, site_dictionary, stage_vol, water_levels, weather,
             overwrite = TRUE, compress = "xz")

    monthly_h2o_bal <- summarise_h2o_bal(lake, weather, lst, isotopes,
                                         water_levels, site_dictionary, stage_vol,
                                         static_gw = FALSE,
                                         median_threshold = 0.01,
                                         static_lake = FALSE,
                                         use_kniffin_pcpn = TRUE)
  } else if (dataset == "kniffin") {
    monthly_weather       <- load_pkg_csv(files$weather_file, filedir)
    monthly_weather$date  <- as_datetime(mdy(monthly_weather$date))
    monthly_weather$P_mm  <- NISTinchTOmeter(monthly_weather$P_in)*1000
    monthly_weather$ET_mm <- NISTinchTOmeter(monthly_weather$E_in)*1000
    monthly_weather$dV    <- 0

    site_dictionary  <- load_pkg_csv(files$site_file, filedir)
    isotopes         <- load_pkg_csv(files$isotope_file, filedir)
    isotopes$date    <- as_datetime(mdy(isotopes$date))
    timeseries       <- isotopes$date
    monthly_isotopes <- summarise_isotopes(isotopes, site_dictionary,
                                           timeseries, static_gw = TRUE,
                                           lake_levels = NULL, gw_levels = NULL,
                                           median_threshold = 0.01,
                                           static_lake = TRUE,
                                           use_kniffin_pcpn = FALSE)
    d18O_evap        <- load_pkg_csv(files$d18O_evap_file, filedir)
    d18O_evap$date   <- as_datetime(mdy(d18O_evap$date))
    monthly_isotopes <- merge(monthly_isotopes, d18O_evap)

    h2o_bal_inputs <- merge(monthly_weather, monthly_isotopes)

    monthly_h2o_bal <- calculate_h2o_bal(h2o_bal_inputs)
  }

  return(monthly_h2o_bal)
}
