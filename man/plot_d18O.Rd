% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_d18O.R
\name{plot_d18O}
\alias{plot_d18O}
\title{Plot d18O}
\usage{
plot_d18O(lake, lake_isotopes, text_size = 12)
}
\arguments{
\item{lake}{lake of interest (e.g., "Pleasant", "Long", or "Plainfield")}

\item{lake_isotopes}{a data frame with the date, lake, site_id, d18O, and d2H of
precip, groundwater, and lake measurements.}

\item{text_size}{size of font, defaults to 12 point}
}
\value{
plot_obj - a plot object with aesthetics added
}
\description{
This function creates a plot object to evaluate the timeseries of d18O
measurements over time at each site.
}
