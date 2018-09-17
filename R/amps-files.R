#' AMPS files
#'
#' Antarctic Mesoscale Prediction System GRIB files.
#'
#' `amps_files` returns all the files, `amps_model_files` returns the files with date set from the file name, `amps_d1files` and `amps_d2files` return only the 30km and 10 km resolution grids respectively.
#'
#' @param time.resolution a placeholder, defaults to "4hourly" and remains unused
#' @param grid one of `d1` (30km resolution) or `d2` (10km resolution)
#' @param ... reserved, unused
#' @export
#' @examples
#' amps_files()
#' amps_model_files()
#' amps_d1files()
#' amps_d2files()
amps_files <- function() {

  files <- dplyr::filter(get_raw_raad_filenames(),
                         stringr::str_detect(.data$file, "ucar"))
  files <- dplyr::filter(files,
                         stringr::str_detect(.data$file,
                         "www2.mmm.ucar.edu"))

  files <- dplyr::filter(files,
                         stringr::str_detect(.data$file,
                                             "wrf_grib"))

  files <- dplyr::filter(files,
                         stringr::str_detect(.data$file,
                                             "grb$"))
  files <- dplyr::filter(files, !stringr::str_detect(basename(.data$file), "^tmp"))

  files

}

#' @name amps_files
#' @export
amps_model_files <- function(time.resolution = "4hourly", grid = "d1", ...) {
  files <- amps_files()
  files$fullname <- file.path(files$root, files$file)
  files <- dplyr::filter(files,
                         stringr::str_detect(.data$file,
                                             sprintf("_%s_", grid)))
  dplyr::transmute(files, hour = substr(basename(.data$fullname), 20, 22),
                   model = substr(basename(.data$fullname), 9, 10),
                   date = as.POSIXct(strptime(basename(files$fullname), "%Y%m%d%H"), tz = "GMT") +
                     as.integer(.data$hour) * 3600, .data$fullname, .data$root) %>%
    set_dt_utc()


}

#' @name amps_files
#' @export
amps_d1files <-
function(time.resolution = "4hourly", ...) {

  files <- amps_model_files(time.resolution = time.resolution,
                            grid = "d1", ...)
  ## TODO normalize file set
  ## we want the most files with the highest preference
  dplyr::mutate(files, prefer = as.integer(.data$hour) > 12, h = as.integer(.data$hour))  %>%
    arrange(desc(.data$prefer), .data$h)   %>% dplyr::mutate(dupe = duplicated(.data$date)) %>% filter(!.data$dupe) %>%
    arrange(.data$date) %>% dplyr::select(.data$date, .data$fullname, .data$root)

}

#' @name amps_files
#' @export
amps_d2files <- function (time.resolution = "4hourly",  ...)
{
  files <- amps_model_files(time.resolution = time.resolution,
                            grid = "d2", ...)

  dplyr::mutate(files, prefer = as.integer(.data$hour) > 12, h = as.integer(.data$hour)) %>%
    arrange(desc(.data$prefer), .data$h) %>% dplyr::mutate(dupe = duplicated(.data$date)) %>%
    filter(!.data$dupe) %>% arrange(.data$date) %>% dplyr::select(
                                                      .data$date, .data$fullname, .data$root)
}
