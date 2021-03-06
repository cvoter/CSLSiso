% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runall_csls_inputs.R
\name{runall_csls_inputs}
\alias{runall_csls_inputs}
\title{Run all functions needed to summarise water budget inputs for a CSLS lake}
\usage{
runall_csls_inputs(
  lake,
  threshold = 0.01,
  start_date = NULL,
  end_date = NULL,
  annual = FALSE
)
}
\arguments{
\item{lake}{name of lake (e.g., Pleasant, Long, or Plainfield)}

\item{threshold}{minimum median difference between lake levels and
groundwater levels during the month of measurement in order
to classify groundwater measurement.}

\item{start_date}{optional start date to use for analysis (first day of the
month) in POSIXct. If not provided, defaults to NULL and
timeseries is based on months with isotope samples.}

\item{end_date}{optional end date to use for analysis (last day of the
month) in POSIXct. If not provided, defaults to NULL and
timeseries is based on months with isotope samples.}

\item{annual}{defaults to FALSE to calculate water balance on a monthly
basis. If TRUE, calculates the annual balance instead.}
}
\value{
monthly_inputs, a data frame with the following columns:
\item{date}{date of month}
\item{ltmp_degC}{average lake surface temperature, degrees C}
\item{ltmp_K}{average lake surface temperature, Kelvin}
\item{atmp_degC}{average air temperature, degrees C}
\item{atmp_K}{average air temperature, Kelvin}
\item{RH_pct}{average relative humidity, percent}
\item{P_mm}{precipitaiton for month or year, mm}
\item{E_mm}{lake evaporation for month or year, mm}
\item{dV_mm}{change in lake volume for month or year, mm}
\item{P_m3}{precipitation for month or year, m3}
\item{E_m3}{lake evaporation for month or year, m3}
\item{dV_m3}{change in lake volume for month or year, m3}
\item{mean_vol_m3}{mean lake volume during the month or year (m3)}
\item{mean_area_m2}{mean lake area during the month or year (m2)}
\item{d18O_lake}{mean stable isotope measurement for d18O, lake (per mil)}
\item{d18O_pcpn}{mean stable isotope measurement for d18O, precipitation
(per mil)}
\item{d18O_GWin}{mean stable isotope measurement for d18O, GWin (per mil)}
\item{d18O_GWout}{mean stable isotope measurement for d18O, GWout (per mil)}
\item{d18O_evap}{mean stable isotope measurement for d18O, evaporation (per
mil)}
\item{d2H_lake}{mean stable isotope measurement for d2H, lake (per mil)}
\item{d2H_pcpn}{mean stable isotope measurement for d2H, precipitation (per
mil)}
\item{d2H_GWin}{mean stable isotope measurement for d2H, GWin (per mil)}
\item{d2H_GWout}{mean stable isotope measurement for d2H, GWout (per mil)}
\item{d2H_evap}{mean stable isotope measurement for d2H, evaporation (per
mil)}
\item{GWin_sites}{name of monitoring wells use for GWin isotopes}
\item{GWout_sites}{name of monitoring wells use for GWout isotopes}
}
\description{
Runs all major functions in the CSLSiso package to transform raw data into
the final inputs data frame for one of the CSLS lakes. Can be calculated on a
monthly basis (annual = FALSE) or based on all available data (annual =
TRUE).
}
\details{
Loaded data includes the following data in a list format, where each lake has
its own data frame. For example, to access groundwater levels for Pleasant
Lake, the command would be \code{gw_levels <- CSLSdata::gw_levels[["Pleasant"]]}.
\itemize{
\item \code{\link[CSLSdata]{weather}}, hourly weather data including date
(POSIXct), atmp (air temperature, deg C), P (precipitation, mm), RH
(relative humidity, percent), Rs (solar radiation, MJ), and wind (wind
speed, m/s)
\item \code{\link[CSLSdata]{lst_HOBO}}, hourly lake surface temperature from
HOBO loggers, including date (POSIXct), ltmp (lake surface temperature,
deg C), and elev_m (elevation of the sensor, meters above mean sea
level)
\item \code{\link[CSLSdata]{isotopes}}, stable isotope measurements including
date (POSIXct), lake, site_id, d18O measurement, and d2H measurement
\item \code{\link[CSLSdata]{lake_levels}}, daily lake levels (meters above
mean sea level) including date (POSIXct), site_no (USGS), obs_type
("LK"), and level_m (level in meters above mean sea level)
\item \code{\link[CSLSdata]{gw_levels}}, daily groundwater levels including
date, site_no, obs_type ("GW"), level_m (level in meters above mean sea
level), and site_id (CSLS site id)
\item \code{\link[CSLSdata]{dictionary}}, links site ids and characteristics
together with columns for lake, obs_type ("LK", "GW", "P"), site_id
(e.g., "LL-01"), SWIMS_station_id, USGS_id, WBIC, static_iso_class
(e.g., "upgradient", "lake", "precipitation"), lat_deg, long_deg,
elev_m, and bouy_bottom_elev_m
}
}
