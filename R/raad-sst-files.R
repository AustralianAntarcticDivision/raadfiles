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
#' oisst_daily_files()
oisst_daily_files <- function() {
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "avhrr"))
  #files <- dplyr::filter(files, grepl("^.*www.ncei.noaa.gov.*sea-surface-temperature-optimum-interpolation.*avhrr-only.*\\.nc$", .data$file))

  # https://github.com/AustralianAntarcticDivision/raadfiles/issues/21
  # OI daily SST files updating now, the path has changed.
  # Previous version (v2) was under the path /www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/access/avhrr-only/
  #
  #   That is moving to /www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2/access/avhrr-only/ BUT only goes to Apr 2020.
  # Superseded by v2.1 which is under the path /www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/
    files <- dplyr::filter(files,     grepl("^.*www.ncei.noaa.gov.*sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/.*\\.nc$", .data$file))

  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), .data$root)

  if (nrow(files) < 1)
    stop("no files found")
 ## datadir <- get_raad_datadir()
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "GMT"))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)  %>%
    dplyr::select(.data$date, .data$fullname, .data$root) %>%
    set_dt_utc()
}
#' @name oisst
#' @export
#' @importFrom raster nlayers stack
#' @importFrom dplyr filter mutate row_number
#' @importFrom stringr str_detect str_replace
#' @importFrom tibble tibble
oisst_monthly_files <- function() {
  files <- dplyr::filter(get_raad_filenames(),
                         stringr::str_detect(.data$file,
                                             "ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"))

  if (nrow(files) < 1) stop("no files found")
  if (nrow(files) > 1) stop("only expecting one file for monthly OIv2 SST, but found ",
                            nrow(files), "please report to the maintainers")

  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), root = .data$root)

  r <- raster::stack(files$fullname, quick = TRUE)
  files <- files[rep(1L, raster::nlayers(r)), ]

  dates <- as.POSIXct(strptime(names(r), "X%Y.%m.%d"), tz  = "GMT")
  dplyr::transmute(files,
                date = dates, fullname  = .data$fullname, band = row_number(), root = .data$root)  %>%
    set_dt_utc()

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
#' oisst_daily_files()
ghrsst_daily_files <- function () {
  files <- dplyr::filter(get_raad_filenames(),
                         stringr::str_detect(.data$file, "ghrsst"))
  files <- dplyr::filter(files, grepl("JPL-L4_GHRSST-SSTfnd-MUR-GLOB.*\\.nc$", .data$file))
  if (nrow(files) < 1)
    stop("no files found")
 # datadir <- get_raad_datadir()
  files <-   dplyr::transmute(files,
                              date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$file), "[0-9]{8}"),"%Y%m%d"),tz = "GMT"),
                              fullname = file.path(.data$root, .data$file),
                              root = .data$root)
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
  files  %>%
    set_dt_utc()
}


