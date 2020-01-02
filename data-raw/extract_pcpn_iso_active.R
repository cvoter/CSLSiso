# Track isotope precipitation collection

# Creates a matrix of date intervals defining when the CSLS precipitation
# collector for isotope measurements was active.

# Setup environment ------------------------------------------------------------
library(lubridate)
library(usethis)

# Extract intervals when precipitation collector was active --------------------
deployment            <- read.csv("data-raw/csls_pcpn_iso_active.csv")
deployment$start_date <- floor_date(mdy_hm(deployment$start_date), unit = "day")
deployment$end_date   <- floor_date(mdy_hm(deployment$end_date), unit = "day")

if (is.na(deployment$end_date[nrow(deployment)])) {
  deployment$end_date[nrow(deployment)] <- today()
}

pcpn_iso_active <- interval(deployment$start_date, deployment$end_date)

# Save data --------------------------------------------------------------------
use_data(pcpn_iso_active, overwrite = TRUE, compress = "xz")
