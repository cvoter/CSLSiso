# Process data from Maribeth Kniffin on Long Lake

# This script takes raw data imported by the package cslsdata and processes it

# Setup environment ------------------------------------------------------------
# Libraries
library(usethis)
library(lubridate)
library(cslsdata)
library(dplyr) # used in isoH2Obudget functions
library(reshape2) # used in isoH2Obudget functions
library(stringr) # used in isoH2Obudget functions
library(zoo) # used in isoH2Obudget functions

# Functions
source("R/summarise_isotopes.R")
source("R/iso_site_type.R")
source("R/timeseries_functions.R")
source("R/iso_lake_gapfill.R")
source("R/iso_pcpn_gapfill.R")
source("R/iso_gw_site_names.R")
source("R/calculate_h2o_bal.R")
source("R/GW_functions.R")

# Data
weather    <- cslsdata::kniffin[["weather"]]
dictionary <- cslsdata::kniffin[["dictionary"]]
isotopes   <- cslsdata::kniffin[["isotopes"]]
d18O_evap  <- cslsdata::kniffin[["d18O_evap"]]
timeseries <- cslsdata::kniffin[["timeseries"]]
# long_lst   <- cslsdata::lst_HOBO[["Long"]]

# Process data -----------------------------------------------------------------
# Update format of timeseries
analysis_intervals <- interval(timeseries,
                               timeseries + months(1) - days(1))
analysis_dates     <- timeseries + months(1) - days(1)
analysis <- list(date = analysis_dates,
                 intervals = analysis_intervals)

# Rearrange isotope data
# Assign site type to each measurement
isotopes <- iso_site_type(isotopes, dictionary, static_gw = TRUE,
                          lake_levels = NULL, gw_levels = NULL, threshold = 0.01)

# Mean d180 by month and site type, drop values w/out site type
isomelt <- isotopes %>%
  group_by(floor_date(.data$date, unit = "month"),
           site_type = .data$site_type) %>%
  filter(is.na(.data$site_type) == FALSE &
           .data$site_type != "") %>%
  summarise(date = floor_date(mean(.data$date), unit = "day"),
            mean_d18O = mean(.data$d18O),
            mean_d2H = mean(.data$d2H)) %>%
  ungroup() %>%
  select(.data$date, .data$site_type,
         .data$mean_d18O, .data$mean_d2H)

# Reshape and rename columns
monthly_isotopes           <- recast(isomelt,
                                     date ~ site_type + variable,
                                     id.var = c("date", "site_type"))
colnames(monthly_isotopes) <- colnames(monthly_isotopes) %>%
  str_replace("downgradient_mean_d18O","d18O_GWout") %>%
  str_replace("upgradient_mean_d18O","d18O_GWin") %>%
  str_replace("precipitation_mean_d18O","d18O_pcpn") %>%
  str_replace("lake_mean_d18O","d18O_lake") %>%
  str_replace("downgradient_mean_d2H","d2H_GWout") %>%
  str_replace("upgradient_mean_d2H","d2H_GWin") %>%
  str_replace("precipitation_mean_d2H","d2H_pcpn") %>%
  str_replace("lake_mean_d2H","d2H_lake")
if (!"d2H_GWout" %in% colnames(monthly_isotopes)) {
  monthly_isotopes$d18O_GWout <- NA
  monthly_isotopes$d2H_GWout  <- NA
}

# Merge data frames
d18O_evap <- fill_timeseries_gaps(d18O_evap, timeseries)
isotopes        <- merge(monthly_isotopes, d18O_evap)
h2o_bal_inputs  <- merge(weather, isotopes)

h2o_bal_inputs$dV_mm <- 0
h2o_bal_inputs$dV_m3 <- 0
h2o_bal_inputs$E_mm  <- h2o_bal_inputs$ET_mm
h2o_bal_inputs$P_m3  <- h2o_bal_inputs$P_mm
h2o_bal_inputs$E_m3  <- h2o_bal_inputs$E_mm

monthly_h2o_bal <- calculate_h2o_bal(h2o_bal_inputs)

kniffin <- list("isotopes" = isotopes,
                "h2o_bal_inputs" = h2o_bal_inputs,
                "monthly_h2o_bal" = monthly_h2o_bal)

use_data(kniffin, overwrite = TRUE, compress = "xz")
