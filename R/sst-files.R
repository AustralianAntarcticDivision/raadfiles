#' OISST v2 files
#'
#' Optimally Interpolated Sea Surface Temperature, from \url{https://www.ncei.noaa.gov/}. These files contain four
#' variables `sst`, `anom`, `err` and `ice` for sea surface temperature, sst anomaly, sst error and sea ice concentration on
#' a regular global longitude latitude grid, with dimensions 1440x720 grid (0.25 degree spatial resolution).
#'
#' At the time of writing (2021-01-18) the files are accessible at
#' \url{https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/}. See the
#' [blueant](https://github.com/AustralianAntarcticDivision/blueant) package for a convenient way to obtain this data set
#' named "NOAA OI 1/4 Degree Daily SST AVHRR".
#'
#' These files can be accessed individually `raster` package function `raster` or as multiple layers with `brick` or `raster::stack`. Use
#' the `varname` argument to choose one of the four variables.
#'
#' To obtain full NetCDF header metadata use 'ncdf4::open.nc(file)' or 'RNetCDF::print.nc(RNetCDF::open.nc(file))' to see
#' the equivalent of 'ncdump -h' output.
#'
#' Optimally Interpolated version 2 SST moved from 'eclipse.ncdc.noaa.gov', to 'www.ncei.noaa.gov' at the end of 2017. Version 2 was superseded by version 2.1 during 2020.
#' @return tibble data frame of file names, with columns `fullname` and `date`
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name oisst
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' \dontrun{
#'   oisst_daily_files()
#' }
oisst_daily_files <- function() {
  pattern <- c("avhrr", "^.*www.ncei.noaa.gov.*sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/.*\\.nc$")
  files <- .find_files_generic(pattern)
  if (nrow(files) < 1) {
    stop("no files found")
  }
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "GMT"))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)  %>%
    dplyr::select(.data$date, .data$fullname, .data$root) %>%
    set_dt_utc()
}
#' @name oisst
#' @export
#' @importFrom dplyr filter mutate row_number
#' @importFrom stringr str_detect str_replace
#' @importFrom tibble tibble
oisst_monthly_files <- function() {
 pattern <- "ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"

  files <- .find_files_generic(pattern)
    if (nrow(files) > 1) warning("only expecting one file for monthly OIv2 SST, but found ",
                            nrow(files), "please report to the maintainers")


  dplyr::transmute(files,
                date = NA, fullname  = .data$fullname, root = .data$root)

}


#' GHRSST files
#'
#' The Group for High Resolution Sea Surface Temperature (GHRSST) files.
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name ghrsst
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' \dontrun{
#'   ghrsst_daily_files()
#' }
ghrsst_daily_files <- function () {
 pattern <- c("ghrsst", "JPL-L4_GHRSST-SSTfnd-MUR-GLOB.*\\.nc$")
 files <- .find_files_generic(pattern)
 # datadir <- get_raad_datadir()
  files <-   dplyr::transmute(files,
                              date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),"%Y%m%d"),tz = "UTC"), .data$fullname, .data$root)
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
}


