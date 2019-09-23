#' Load csv from isoH2Obudget Installation
#'
#'
#' This function loads a csv file that by default resides in the installed
#' isoH2Obudget package (saved to inst/extdata during package creation). The
#' file location can be changed by providing a different file directory
#' argument.
#'
#' @param filename name of the csv file to load (e.g., "input_data.csv")
#' @param filedir name of file directory to search for this csv file. Defaults
#'                to 'system.file' to search the inst/extdata location of the
#'                isoH2Obudget package
#' @param skip_lines number of rows in csv file to skip before reading in.
#'                   Defaults to zero.
#'
#' @return df, a data frame created from the csv file
#'
#' @importFrom utils read.csv
#'
#' @export

load_pkg_csv <- function(filename,
                         filedir = 'system.file',
                         skip_lines = 0){
  if (filedir == 'system.file') {
    df <- read.csv(system.file("extdata",
                               filename,
                               package = "isoH2Obudget",
                               mustWork = TRUE),
                   skip = skip_lines)
  } else {
    df <- read.csv(sprintf("%s/%s", filedir, filename), skip = skip_lines)
  }

  return(df)
}
