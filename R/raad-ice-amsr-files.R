#' AMSR daily  sea-ice concentration
#'
#' Sea ice concentration files at 6.25 km resolution, southern hemisphere.
#'
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name oisst
#' @export
#' @importFrom stringr str_detect str_extract
#' @export
#' @examples
#' amsr_daily_files()
amsr_daily_files <- function() {
  files <- dplyr::filter(get_raw_raad_filenames(),
                         stringr::str_detect(.data$file, "s6250"))

  files <- dplyr::filter(files, stringr::str_detect(.data$file, "hdf$"))
  ## 2002:2011
    #  f1 <- "ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/"
    ## modified from zmaw.de 2017-06-27 https://github.com/AustralianAntarcticDivision/raadtools/issues/52
    f1 <- "ftp-projects.cen.uni-hamburg.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/"
    ## 2012:2015+
    f2 <- "www.iup.uni-bremen.de\\+8084/amsr2data/asi_daygrid_swath/s6250"
    files <- dplyr::filter(files,
                             stringr::str_detect(.data$file, f1) |
                             stringr::str_detect(.data$file, f2))
 files <- files %>% mutate(date = as.POSIXct(as.Date(stringr::str_extract(.data$file, "[0-9]{8}"), "%Y%m%d"), tz = "GMT"))
    files %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
}
