#' BOM tmax daily fils
#'
#' Data is daily but arranged in monthly files.
#'
#' @return
#' @export
#'
#' @examples
bom_tmax_daily_files <- function() {
  files <- dplyr::filter(get_raw_cfa_filenames(), stringr::str_detect(.data$file, "CFA/BoM_daily_vars/tmax"))
  files <-   dplyr::filter(files, stringr::str_detect(.data$file, "tmax_day.*\\.nc$"))
  files <-   dplyr::transmute(files, file = .data$file, fullname = file.path(.data$root, .data$file))

  if (nrow(files) < 1)
    stop("no files found")
  datadir <- get_cfa_datadir()
  ## need to expand this into a band per slice
  ## or figure out the new tidync approach
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "GMT"),
                         file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
}


get_raw_cfa_filenames <- function() {
  getOption("cfafiles.filename.database" )
}
get_cfa_datadir <- function() {
  getOption("cfafiles.default.data.directory")
}
