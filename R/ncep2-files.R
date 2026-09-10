##.find_files_generic("ncep.reanalysis2.dailyavgs")


#' NCEP2 wind files
#'
#' NCEP2 six-hourly reanalysis2 gaussian grid
#'
#' @return tibble data frame of file names
#' @name ncep2_files
#' @export
#' @examples
#' \dontrun{
#'   ncep2_uwnd_6hr_files()
#'   ncep2_vwnd_6hr_files()
#' }
ncep2_uwnd_6hr_files <- function() {
  pattern <- c("ncep.reanalysis2/gaussian_grid", "^.*uwnd.*gauss.*\\.nc$")
  files <- .find_files_generic(pattern)

  if (nrow(files) < 1)
    stop("no files found")
  files <- dplyr::transmute(files, date = ISOdate(as.integer(stringr::str_extract(basename(.data$fullname), "[0-9]{4}")), 1, 1, 0, 0, 0, tz = "UTC"),
                         fullname = .data$fullname, root = .data$root)
  .raad_files_result(dplyr::arrange(dplyr::distinct(files, .data$date, .keep_all = TRUE), .data$date))

}
#' @name ncep2_files
#' @export
ncep2_vwnd_6hr_files <- function() {
  pattern <- c("ncep.reanalysis2/gaussian_grid", "^.*vwnd.*gauss.*\\.nc$")
  files <- .find_files_generic(pattern)

  if (nrow(files) < 1)
    stop("no files found")
  files <- dplyr::transmute(files, date = ISOdate(as.integer(stringr::str_extract(basename(.data$fullname), "[0-9]{4}")), 1, 1, tz  = "UTC"),
                         fullname = .data$fullname, root = .data$root)
  .raad_files_result(dplyr::arrange(dplyr::distinct(files, .data$date, .keep_all = TRUE), .data$date))


}




