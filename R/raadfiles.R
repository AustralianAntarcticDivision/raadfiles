#' OISST files
#'
#' Optimally Interpolated Sea Surface Temperature
#'
#' @return tibble data frame of file names
#' @export
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' oisst_daily_files()
oisst_daily_files <- function() {
  datadir <- getOption("raadfiles.default.data.directory")
  files <- dplyr::filter(getOption("raadfiles.filename.database" ), stringr::str_detect(.data$fullname, "avhrr"))
  files <- dplyr::filter(files, grepl("^.*eclipse\\.ncdc\\.noaa\\.gov.*OI-daily-v2.*\\.nc$", .data$fullname))
  if (nrow(files) < 1)
    stop("no files found")
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                   "%Y%m%d"),tz = "GMT"),
                   file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
 }
