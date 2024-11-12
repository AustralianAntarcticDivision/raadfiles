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

  pattern <- c("cafe.modis.r2018", "^.*orca.science.oregonstate.edu.*/cafe.*\\.hdf$")
  files <- .find_files_generic(pattern)

  if (nrow(files) < 1)
    stop("no files found")
  ## datadir <- get_raad_datadir()
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{7}"),
                                                          "%Y%j"),tz = "UTC"))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)  %>%
    dplyr::select(.data$date, .data$fullname, .data$root)

}
