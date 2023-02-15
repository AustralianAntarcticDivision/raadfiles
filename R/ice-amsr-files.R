
#' @name amsr_daily_files
#' @export
amsr2_3k_daily_files <- function() {
  pattern <- c("s3125", "tif$", "seaice.uni-bremen.de/data/amsr2/asi_daygrid_swath/s3125.*Antarctic3125")

  files <- .find_files_generic(pattern)
  files <- files %>% mutate(date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"), "%Y%m%d"), tz = "UTC"))
  distinct(arrange(files, .data$date), .data$date, .keep_all = TRUE)

}




#' @name amsr_daily_files
#' @export
amsre_daily_files <- function() {
 pattern <- c("s6250", "hdf$", "ftp-projects.cen.uni-hamburg.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/")

  files <- .find_files_generic(pattern)
  files <- files %>% mutate(date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"), "%Y%m%d"), tz = "UTC"))
  distinct(arrange(files, .data$date), .data$date, .keep_all = TRUE)

}

#' @name amsr_daily_files
#' @export
amsr2_daily_files <- function() {
  pattern <- c("s6250", "tif$", "seaice.uni-bremen.de/data/amsr2/asi_daygrid_swath/s6250.*Antarctic/")

  files <- .find_files_generic(pattern)
  files <- files %>% mutate(date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"), "%Y%m%d"), tz = "UTC"))
  distinct(arrange(files, .data$date), .data$date, .keep_all = TRUE)

}





#' AMSR daily  sea-ice concentration
#'
#' Sea ice concentration files at 6.25 km resolution, southern hemisphere.
#'
#' `amsre_daily_files()` returns HDF files
#'
#' `amsr2_daily_files()` returns TIF files
#'
#' `amsr2_3k_daily_files()` returns TIF files
#'
#' `amsr_daily_files()` returns HDF files
#'
#' The HDF files require flipping about the y-axis, and setting the polar extent and the crs (projection)
#' metadata. The TIF files don't require this, they are completely oriented and metadata-complete.
#'
#' @name amsr_daily_files
#' @aliases amsre_daily_files amsr2_daily_files amsr2_3k_daily_files
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate transmute
#' @importFrom rlang .data
#' @export
#' @importFrom stringr str_detect str_extract
#' @export
#' @examples
#' \dontrun{
#'   ## this combines amsr2 (2012-) and amsre (2002-2011)
#'   amsr_daily_files()
#' }
amsr_daily_files <- function() {
  #  ## 2002:2011
  #    #  f1 <- "ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/"
  #    ## modified from zmaw.de 2017-06-27 https://github.com/AustralianAntarcticDivision/raadtools/issues/52
  #    f1 <- "ftp-projects.cen.uni-hamburg.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/"
  #    ## 2012:2015+
  #    f2 <- "seaice.uni-bremen.de/data/amsr2/asi_daygrid_swath/s6250"

  pattern <- c("s6250", "hdf$")
  f1 <- "ftp-projects.cen.uni-hamburg.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/"
  f2 <- "seaice.uni-bremen.de/data/amsr2/asi_daygrid_swath/s6250"

  files <- .find_files_generic(pattern)
  files <- dplyr::bind_rows(dplyr::filter(files, stringr::str_detect(.data$fullname, f1) |
                                                  stringr::str_detect(.data$fullname, f2)))

  files <- files %>% mutate(date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"), "%Y%m%d"), tz = "UTC"))
  distinct(arrange(files, .data$date), .data$date, .keep_all = TRUE)


}
