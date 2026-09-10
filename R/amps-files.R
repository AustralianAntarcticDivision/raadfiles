#' AMPS files
#'
#' Antarctic Mesoscale Prediction System GRIB files.
#'
#' `amps_files` returns all the files, `amps_model_files` returns the files with date set from the file name, `amps_d1_files` and `amps_d2_files` return (`amps_d1files`/`amps_d2files` are older aliases) only the 30km and 10 km resolution grids respectively.
#'
#' @param time.resolution a placeholder, defaults to "4hourly" and remains unused
#' @param grid one of `d1` (30km resolution) or `d2` (10km resolution)
#' @param ... reserved, unused
#' @export
#' @examples
#' \dontrun{
#'   amps_files()
#'   amps_model_files()
#'   amps_d1_files()
#'   amps_d2_files()
#' }
amps_files <- function() {
  files <- .find_files_generic(c("www2.mmm.ucar.edu", "wrf_grib", "grb$"))
  files <- files[!startsWith(basename(files$fullname), "tmp"), ]
  if (nrow(files) < 1) stop("no files found")
  .raad_files_result(files)
}

#' @name amps_files
#' @export
amps_model_files <- function(time.resolution = "4hourly", grid = "d1", ...) {
  files <- amps_files()
  files <- dplyr::filter(files,
                         stringr::str_detect(.data$fullname,
                                             sprintf("_%s_", grid)))
  .raad_files_result(dplyr::transmute(files, hour = substr(basename(.data$fullname), 20, 22),
                   model = substr(basename(.data$fullname), 9, 10),
                   date = as.POSIXct(strptime(basename(files$fullname), "%Y%m%d%H"), tz = "UTC") +
                     as.integer(.data$hour) * 3600, .data$fullname, .data$root))

}

#' @name amps_files
#' @export
amps_d1_files <-
function(time.resolution = "4hourly", ...) {

  files <- amps_model_files(time.resolution = time.resolution,
                            grid = "d1", ...)
  ## TODO normalize file set
  ## we want the most files with the highest preference
  dplyr::mutate(files, prefer = as.integer(.data$hour) > 12, h = as.integer(.data$hour))  %>%
    arrange(desc(.data$prefer), .data$h)   %>% dplyr::mutate(dupe = duplicated(.data$date)) %>% filter(!.data$dupe) %>%
    arrange(.data$date) %>% dplyr::select("date", "fullname", "root")

}

#' @name amps_files
#' @export
amps_d2_files <- function (time.resolution = "4hourly",  ...)
{
  files <- amps_model_files(time.resolution = time.resolution,
                            grid = "d2", ...)

  dplyr::mutate(files, prefer = as.integer(.data$hour) > 12, h = as.integer(.data$hour)) %>%
    arrange(desc(.data$prefer), .data$h) %>% dplyr::mutate(dupe = duplicated(.data$date)) %>%
    filter(!.data$dupe) %>% arrange(.data$date) %>% dplyr::select(
                                                      "date", "fullname", "root")
}

#' @name amps_files
#' @export
amps_d1files <- function(...) amps_d1_files(...)
#' @name amps_files
#' @export
amps_d2files <- function(...) amps_d2_files(...)
