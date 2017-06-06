#' OISST files
#'
#' Optimally Interpolated Sea Surface Temperature
#'
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name oisst
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' oisst_daily_files()
oisst_daily_files <- function() {
  files <- dplyr::filter(get_raw_raad_filenames(), stringr::str_detect(.data$file, "avhrr"))
  files <- dplyr::filter(files, grepl("^.*eclipse\\.ncdc\\.noaa\\.gov.*OI-daily-v2.*\\.nc$",
                                      .data$file))
  files <-   dplyr::transmute(files, file = .data$file, fullname = file.path(.data$root, .data$file))

  if (nrow(files) < 1)
    stop("no files found")
  datadir <- get_raad_datadir()
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "GMT"),
                         file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
}
#' @name oisst
#' @export
#' @importFrom raster nlayers stack
#' @importFrom dplyr filter mutate row_number
#' @importFrom stringr str_detect str_replace
#' @importFrom tibble tibble
oisst_monthly_files <- function() {
  files <- dplyr::filter(get_raw_raad_filenames(),
                         stringr::str_detect(.data$file,
                                             "ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"))

  if (nrow(files) < 1) stop("no files found")
  if (nrow(files) > 1) stop("only expecting one file for monthly OIv2 SST, but found ",
                            nrow(files), "please report to the maintainers")

  files <-   dplyr::transmute(files, file = .data$file, fullname = file.path(.data$root, .data$file))

  r <- raster::stack(files$fullname, quick = TRUE)
  files <- files[rep(1L, raster::nlayers(r)), ]

  dates <- as.POSIXct(strptime(names(r), "X%Y.%m.%d"), tz  = "GMT")
  datadir <- get_raad_datadir()
  dplyr::mutate(files, file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""),
                date = dates, band = row_number())

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
  files <- dplyr::filter(get_raw_raad_filenames(),
                         stringr::str_detect(.data$file, "ghrsst"))
  files <- dplyr::filter(files, grepl("JPL-L4_GHRSST-SSTfnd-MUR-GLOB.*\\.nc$", .data$file))
  if (nrow(files) < 1)
    stop("no files found")
  datadir <- get_raad_datadir()
  files <-   dplyr::transmute(files, file = .data$file, fullname = file.path(.data$root, .data$file))

  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "GMT"),
                         file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
  files
}


