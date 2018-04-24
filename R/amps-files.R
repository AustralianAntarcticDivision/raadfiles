#' AMPS files
#'
#' Antarctic Mesoscale Prediction System
#'
#'
#' @export
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
  ## get all of them at this level
  #files <- dplyr::filter(files,
  #                       stringr::str_detect(.data$file,
  #                                           sprintf("_%s_", grid)))

  files

}


amps_model_files <- function(data.source = "", time.resolution = "4hourly", grid = "d1", ...) {
  files <- raadfiles::amps_files()
  #datadir <- getOption("default.datadir")
  files$fullname <- file.path(files$root, files$file)
  files <- dplyr::filter(files,
                         stringr::str_detect(.data$file,
                                             sprintf("_%s_", grid)))
  dplyr::transmute(files, hour = substr(basename(fullname), 20, 22),
                   model = substr(basename(fullname), 9, 10),
                   date = as.POSIXct(strptime(basename(files$fullname), "%Y%m%d%H"), tz = "GMT") +
                     as.integer(hour) * 3600, fullname, file)


}

#' @name amps_files
#' @export
amps_d1files <-
function(data.source = "", time.resolution = "4hourly", ...) {

  files <- amps_model_files(data.source = data.source, time.resolution = time.resolution,
                            grid = "d1", ...)
  ## TODO normalize file set
  ## we want the most files with the highest preference
  dplyr::mutate(files, prefer = as.integer(hour) > 12, h = as.integer(hour))  %>%
    arrange(desc(prefer), h)   %>% dplyr::mutate(dupe = duplicated(date)) %>% filter(!dupe) %>%
    arrange(date) %>% dplyr::select(file, date, fullname)

}

## original version for d1 in raadtools, needs to be moved over
## raadtools::amps_d1files

#' @name amps_files
#' @export
amps_d2files <- function (data.source = "", time.resolution = "4hourly",  ...)
{
  files <- amps_model_files(data.source = data.source, time.resolution = time.resolution,
                            grid = "d2", ...)

  dplyr::mutate(files, prefer = as.integer(hour) > 12, h = as.integer(hour)) %>%
    arrange(desc(prefer), h) %>% dplyr::mutate(dupe = duplicated(date)) %>%
    filter(!dupe) %>% arrange(date) %>% dplyr::select(file,
                                                      date, fullname)
}
