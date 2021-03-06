library(CSLSiso)
library(NISTunits)

test_that("d18O_evap functions replicate Krabbenhoft 1990 results", {

  ltmp      <- c(4, 11.9, 18.1, 22.2, 22.3, 18.1, 11.3, 6.0)
  atmp      <- c(3.7, 11.8, 16.0, 19.4, 18.7, 12.7, 6.3, -1.3)
  d18O_pcpn <- c(-14.8, -10.7, -6.3, -5.1, -5.3, -8.4, -11.1, -13.8)
  d18O_evap <- c(-3.95, -13.90, -25.24, -23.97, -17.13, -10.40, -6.12, 33.24)
  h         <- c(0.76, 0.75, 0.78, 0.84, 0.86, 0.86, 0.82, 0.90)
  alphas    <- c(0.98878, 0.98956, 0.98994, 0.99024, 0.99018, 0.98964, 0.98904,
                 0.98826)
  del_eps   <- c(3.36, 3.36, 3.12, 2.27, 1.96, 1.96, 2.62, 1.43)
  epsilon   <- c(14.68, 14.07, 13.17, 12.04, 11.78, 12.31, 13.58, 13.17)

})
