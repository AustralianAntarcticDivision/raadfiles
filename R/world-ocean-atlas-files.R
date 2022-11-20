#' World Ocean Atlas  products
#'
#' WOA find files
#'
#'   Current returns all NetCDF files, without any date information, there's a mix of variables month/year climatologies.
#'
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name WOA
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' \dontrun{
#'   woa13_files()
#' }
woa13_files <- function() {
  ## https://github.com/AustralianAntarcticDivision/raadtools/issues/53#issuecomment-311489621
pattern <- c("data.nodc.noaa.gov/woa/WOA13/DATAv2", ".*nc$")
files <- .find_files_generic(pattern)
files
}

#' @export
#' @name WOA
woa09_files <- function() {
  .Defunct("woa13_files")

}

#' @export
#' @name WOA
woa09_daily_files <- function() {
 .Defunct("woa09_files")
}
