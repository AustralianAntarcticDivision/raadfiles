#' Southern Ocean State Estimate files
#'
#' Files from Ocean State Estimation at Scripps for the Southern Ocean 'SOSE'.
#'
#' Iteration provided is latest available, otherwise this argument will be used to match with file names.
#' @param iteration default is '' which finds latest available, see details
#' @return data frame of file names and date, 'date', 'fullname'
#'
#' @export
#' @references http://sose.ucsd.edu/
#' @examples
#' sose_monthly_files()
sose_monthly_files <- function(iteration = "") {
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "sose.ucsd.edu"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, ".*sose.ucsd.edu.*monthly.*nc$"))
  iters_available <- unique(stringr::str_extract(files$file, "ITER[0-9][0-9][0-9]"))

  if (iteration == "") {
    files <- dplyr::filter(files, stringr::str_detect(file, max(iters_available)))
  } else {
    if (!iteration %in% iters_available) {
      stop(sprintf('cannot find iteration %s', iteration))
    }
    files <- dplyr::filter(files, stringr::str_detect(file, iteration))
  }
  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), .data$root)

  if (nrow(files) < 1)
    stop("no files found")

 files
}
