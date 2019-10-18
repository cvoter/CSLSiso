#' Run isoH2Obudget Start to Finish
#'
#' Runs all major functions in the isoH2Obudget package, from loading in raw
#' data to the final monthly water balance
#'
#' @param lake name of lake (e.g., Pleasant, Long, or Plainfield)
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
#' @importFrom usethis use_data
#' @importFrom NISTunits NISTinchTOmeter
#' @import lubridate
#' @import cslsdata
#'
#' @export

runall_isoH2Obudget <- function(lake, dataset = "csls"){
  if (dataset == "csls") {
    weather <- cslsdata::weather
    if (lake == "Pleasant") {
      lst             <- cslsdata::psnt_lst
      isotopes        <- cslsdata::psnt_isotopes
      lake_levels     <- cslsdata::psnt_lake_levels
      gw_levels       <- cslsdata::psnt_gw_levels
      stage_vol       <- cslsdata::psnt_stage_vol
      site_dictionary <- cslsdata::psnt_dictionary

    } else if (lake == "Long") {
      lst             <- cslsdata::long_lst
      isotopes        <- cslsdata::long_isotopes
      lake_levels     <- cslsdata::long_lake_levels
      gw_levels       <- cslsdata::long_gw_levels
      stage_vol       <- cslsdata::long_stage_vol
      site_dictionary <- cslsdata::long_dictionary

    } else if (lake == "Plainfield") {
      lst             <- cslsdata::pfl_lst
      isotopes        <- cslsdata::pfl_isotopes
      lake_levels     <- cslsdata::pfl_lake_levels
      gw_levels       <- cslsdata::pfl_gw_levels
      stage_vol       <- cslsdata::pfl_stage_vol
      site_dictionary <- cslsdata::pfl_dictionary

    }
    monthly_h2o_bal <- summarise_h2o_bal(lake, weather, lst,
                                         isotopes, lake_levels,
                                         gw_levels, site_dictionary,
                                         stage_vol, static_gw = FALSE,
                                         median_threshold = 0.01,
                                         static_lake = FALSE,
                                         use_kniffin_pcpn = TRUE)
  } else if (dataset == "kniffin") {
    # monthly_weather       <- load_pkg_csv(files$weather_file, filedir)
    # monthly_weather$date  <- as_datetime(mdy(monthly_weather$date))
    # monthly_weather$P_mm  <- NISTinchTOmeter(monthly_weather$P_in)*1000
    # monthly_weather$ET_mm <- NISTinchTOmeter(monthly_weather$E_in)*1000
    # monthly_weather$dV    <- 0
    #
    # site_dictionary  <- load_pkg_csv(files$site_file, filedir)
    # isotopes         <- load_pkg_csv(files$isotope_file, filedir)
    # isotopes$date    <- as_datetime(mdy(isotopes$date))
    # timeseries       <- isotopes$date
    # monthly_isotopes <- summarise_isotopes(isotopes, site_dictionary,
    #                                        timeseries, static_gw = TRUE,
    #                                        lake_levels = NULL, gw_levels = NULL,
    #                                        median_threshold = 0.01,
    #                                        static_lake = TRUE,
    #                                        use_kniffin_pcpn = FALSE)
    # d18O_evap        <- load_pkg_csv(files$d18O_evap_file, filedir)
    # d18O_evap$date   <- as_datetime(mdy(d18O_evap$date))
    # monthly_isotopes <- merge(monthly_isotopes, d18O_evap)
    #
    # h2o_bal_inputs <- merge(monthly_weather, monthly_isotopes)
    #
    # monthly_h2o_bal <- calculate_h2o_bal(h2o_bal_inputs)
  }

  return(monthly_h2o_bal)
}
