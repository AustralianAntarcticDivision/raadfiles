#' SMAP ocean surface salinity files
#'
#' Remote Sensing Systems SMAP Level 3 Sea Surface Salinity Standard Mapped Image 8day running
#'
#' @return tibble data frame of file names, with columns `fullname` and `date`
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name smap
#' @aliases salt
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' smap_daily_files()
smap_8day_files <- function() {
  #ftp://podaac-ftp.jpl.nasa.gov/allData/smap/L3/RSS/V3/8day_running/SCI/40KM/2015/272/
  
  ##  RSS_smap_SSS_L3_8day_running_40km_2016_079_FNL_v03.0.nc
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "RSS_smap_SSS_L3_8day_running_40km"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, ".*\\.nc$"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, "podaac-ftp.jpl.nasa.gov"))
  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), .data$root)
  
  if (nrow(files) < 1)
    stop("no smap files found")
  ## datadir <- get_raad_datadir()
  jday <- basename(dirname(files$fullname))
  Y <- basename(dirname(dirname(files$fullname)))

files$date <- as.POSIXct(strptime(sprintf("%s-%s", Y, jday), "%Y-%j"), tz = "UTC") #as.POSIXct(strptime(basename(.data$fullname), "RSS_smap_SSS_L3_8day_running_40km"), tz = "UTC"))
  files <- dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)  %>%
    dplyr::select(.data$date, .data$fullname, .data$root) %>%
    raadfiles:::set_dt_utc()
}
