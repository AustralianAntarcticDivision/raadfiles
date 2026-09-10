#' World Ocean Atlas  products
#'
#' WOA find files
#'
#'   Current returns all NetCDF files, without any date information, there's a mix of variables month/year climatologies.
#'
#' @return tibble data frame of file names
#' @name WOA
#' @export
#' @examples
#' \dontrun{
#'   woa13_files()
#' }
woa13_files <- function() {
  ## https://github.com/AustralianAntarcticDivision/raadtools/issues/53#issuecomment-311489621
  pattern <- c("data.nodc.noaa.gov/woa/WOA13/DATAv2", ".*nc$")
  .raad_files_result(.find_files_generic(pattern))
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
