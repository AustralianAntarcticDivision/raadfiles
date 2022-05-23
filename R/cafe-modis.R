#' 'Cafe' MODIS files
#'
#'
#'
#' @return data frame of file names and date, 'date', 'fullname'
#' @export
#'
#' @examples
#' \dontrun{
#'   cafe_monthly_files()
#' }
cafe_monthly_files <- function() {
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "cafe.modis.r2018"))
  files <- dplyr::filter(files, grepl("^.*orca.science.oregonstate.edu.*/cafe.*\\.hdf$",
                                      .data$file))
  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), .data$root)

  if (nrow(files) < 1)
    stop("no files found")
  ## datadir <- get_raad_datadir()
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{7}"),
                                                          "%Y%j"),tz = "UTC"))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)  %>%
    dplyr::select(.data$date, .data$fullname, .data$root) %>%
    set_dt_utc()

}
