#' AMPS files
#'
#' Antarctic Mesoscale Prediction System
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

  files <- dplyr::filter(files,
                         stringr::str_detect(.data$file,
                                             "d1"))

  files

}
