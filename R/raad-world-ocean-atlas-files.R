#' World Ocean Atlas  products
#'
#' WOA09 incomplete function
#'
#'   Current returns all NetCDF files, without any date.
#'
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name altimetry
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' woa09_daily_files()
woa09_daily_files <- function() {
  files <- dplyr::filter(get_raw_raad_filenames(), stringr::str_detect(.data$file, "data.nodc.noaa.gov"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, ".*nc$"))

  files <-   dplyr::transmute(files, file = .data$file, fullname = file.path(.data$root, .data$file))

  if (nrow(files) < 1)
    stop("no files found")
  datadir <- get_raad_datadir()
  files <- dplyr::mutate(files,
                         #date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                        #                                  "%Y%m%d"),tz = "GMT"),

                         file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""))
  #files$date[is.na(files$date)] <- max(files$date, na.rm = TRUE) + 24 * 3600
  #dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
files
  }
