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
#' woa13_files()
woa13_files <- function() {
  ## https://github.com/AustralianAntarcticDivision/raadtools/issues/53#issuecomment-311489621
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "data.nodc.noaa.gov/woa/WOA13/DATAv2"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, ".*nc$"))

  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), root = .data$root)
  if (nrow(files) < 1)
    stop("no files found")


files
}

#' @export
#' @name WOA
woa09_files <- function() {
  .Deprecated("woa13_files")
  ## https://github.com/AustralianAntarcticDivision/raadtools/issues/53#issuecomment-311489621
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "data.nodc.noaa.gov/woa/WOA09"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, ".*nc$"))

  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), root = .data$root)

  if (nrow(files) < 1)
    stop("no files found")


  files
}

#' @export
#' @name WOA
woa09_daily_files <- function() {
 .Defunct("woa09_files")
}
