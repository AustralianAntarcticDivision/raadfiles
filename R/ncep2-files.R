#' NCEP2 wind files
#'
#' NCEP2 six-hourly reanalysis2 gaussian grid
#'
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name ncep2_files
#' @export
#' @importFrom stringr str_extract str_replace
#' @examples
#' ncep2_uwnd_6hr_files()
ncep2_uwnd_6hr_files <- function() {
  files <- ncep2_6hr_files()
  files <- dplyr::filter(files, grepl("^.*uwnd.*gauss.*\\.nc$", .data$file))
  files <-   dplyr::transmute(files, file = .data$file, fullname = file.path(.data$root, .data$file))

  if (nrow(files) < 1)
    stop("no files found")
  datadir <- get_raad_datadir()
  files <- dplyr::mutate(files, date = ISOdate(as.integer(stringr::str_extract(file, "[0-9]{4}")), 1, 1),
                         file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""))
  dplyr::arrange(dplyr::distinct(files, date, varname,  .keep_all = TRUE), date)
}
#' @name ncep2_files
#' @export
ncep2_vwnd_6hr_files <- function() {
  files <- ncep2_6hr_files()
  files <- dplyr::filter(files, grepl("^.*vwnd.*gauss.*\\.nc$", .data$file))
  files <-   dplyr::transmute(files, file = .data$file, fullname = file.path(.data$root, .data$file))

  if (nrow(files) < 1)
    stop("no files found")
  datadir <- get_raad_datadir()
  files <- dplyr::mutate(files, date = ISOdate(as.integer(stringr::str_extract(file, "[0-9]{4}")), 1, 1),
                         file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""))
  dplyr::arrange(dplyr::distinct(files, date, varname,  .keep_all = TRUE), date)
}

#dplyr::filter(get_raw_raad_filenames(), stringr::str_detect(.data$file, "ncep.reanalysis2")) %>% mutate(file = basename(dirname(file))) %>% distinct(file)
# A tibble: 5 x 1
#file
#<chr>
#1              gaussian_grid
#2                   pressure
#3 ncep.reanalysis2.dailyavgs
#4                    surface
#5   ncep.reanalysis2.derived


ncep2_6hr_files <- function() {
  dplyr::filter(get_raw_raad_filenames(), stringr::str_detect(.data$file, "ncep.reanalysis2/gaussian_grid"))
}
ncep2_dailyavgs_files <- function() {
  dplyr::filter(get_raw_raad_filenames(), stringr::str_detect(.data$file, "ncep.reanalysis2.dailyavgs"))
}
