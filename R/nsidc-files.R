#' NSIDC daily and monthly sea-ice concentration
#'
#' Sea ice concentration files.
#'
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name nsidc
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @export
#' @examples
#' nsidc_south_monthly_files()
#' nsidc_north_monthly_files()
#' nsidc_monthly_files()
nsidc_south_monthly_files <- function() {
  files <-    dplyr::filter(nsidc_monthly_files(), stringr::str_detect(file, "south"))
    ## arrange and distinct to resolve versions
  files <-  dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(file)), date, .keep_all = TRUE), date)
  if (nrow(files) < 1)
    stop("no files found")
  files
}
#' @name nsidc
#' @export
nsidc_north_monthly_files <- function() {
  files <-    dplyr::filter(nsidc_monthly_files(), stringr::str_detect(file, "north"))
  ## arrange and distinct to resolve versions
  files <-  dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(file)), date, .keep_all = TRUE), date)
  if (nrow(files) < 1)
    stop("no files found")
  files
}
#' @name nsidc
#' @export
nsidc_monthly_files <- function() {
  files <- dplyr::filter(get_raw_raad_filenames(), stringr::str_detect(.data$file, "sidads"))
 files <- dplyr::filter(files, stringr::str_detect(.data$file, "nsidc0051_gsfc_nasateam_seaice.*monthly.*bin$"))
  files <-   dplyr::transmute(files, file = .data$file,
                              fullname = file.path(.data$root, .data$file))

  datadir <- get_raad_datadir()
  files <-
    dplyr::mutate(files, date = as.POSIXct(as.Date(sprintf("%s01", stringr::str_sub(basename(.data$fullname), 4, 9)),
                                                   "%Y%m%d"),
                                           tz = "GMT"),
                         file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""))
  files   %>%
    set_dt_utc()
}
#' @name nsidc
#' @export
nsidc_south_daily_files <- function() {
  files <-    dplyr::filter(nsidc_daily_files(), stringr::str_detect(file, "south"))
  ## arrange and distinct to resolve versions
  files <-  dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(file)), date, .keep_all = TRUE), date)
  if (nrow(files) < 1)
    stop("no files found")
  files
}
#' @name nsidc
#' @export
nsidc_north_daily_files <- function() {
  files <-    dplyr::filter(nsidc_daily_files(), stringr::str_detect(file, "north"))
  ## arrange and distinct to resolve versions
  files <-  dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(file)), date, .keep_all = TRUE), date)
  if (nrow(files) < 1)
    stop("no files found")
  files
}
#' @name nsidc
#' @export
nsidc_daily_files <- function() {
  files <- dplyr::filter(get_raw_raad_filenames(), stringr::str_detect(.data$file, "sidads"))

  files <- dplyr::filter(files, stringr::str_detect(.data$file, "bin$"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, "v1.1") | stringr::str_detect(.data$file, "f18_nrt"))

  files <- dplyr::filter(files, stringr::str_detect(.data$file, "nsidc0051_gsfc_nasateam_seaice.*daily") | stringr::str_detect(.data$file, "nsidc0081_nrt_nasateam_seaice"))
  files <-   dplyr::transmute(files, file = .data$file,
                              fullname = file.path(.data$root, .data$file))

  datadir <- get_raad_datadir()
  files <-
    dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_sub(basename(.data$fullname), 4, 11), "%Y%m%d"),
                                           tz = "GMT"),
                  file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""))

  files   %>%
    set_dt_utc()
}
