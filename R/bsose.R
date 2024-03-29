#' @importFrom utils tail

sose_iters <- function() {
  pattern <- c("sose.ucsd.edu", ".*sose.ucsd.edu.*monthly.*nc$")
  files <- .find_files_generic(pattern)
  unique(stringr::str_extract(files$file, "ITER[0-9][0-9][0-9]"))
}

sose_vars <- function() {
  pattern <- c("sose.ucsd.edu", ".*sose.ucsd.edu.*monthly.*nc$")
  files <- .find_files_generic(pattern)

  sort(unique(gsub("\\.nc$", "", unlist(lapply(strsplit(files$file, "_"), tail, 1L)))))
}

#' Southern Ocean State Estimate files
#'
#' Files from Ocean State Estimation at Scripps for the Southern Ocean 'SOSE'.
#'
#' Iteration provided is latest available, otherwise this argument will be used to match with file names.
#'
#' Dates in the files are extracted and expanded out for every time step, it's assumed this will be used in raadtools
#' along with a 'level' argument to return a time step or time series.
#' @param varname default is '' which is the first available, set to 'all' to return all file names without date expansion
#' @param iteration default is '' which finds latest available, see details
#' @return data frame of file names and date, 'date', 'fullname'
#'
#' @export
#' @references http://sose.ucsd.edu/
#' @examples
#'   sose_monthly_files()
sose_monthly_files <- function(varname = "", iteration = "") {
  pattern <- c("sose.ucsd.edu", ".*sose.ucsd.edu.*monthly.*nc$")
  files <- .find_files_generic(pattern)

  iters_available <- unique(stringr::str_extract(files$fullname, "ITER[0-9][0-9][0-9]"))

  if (iteration == "") {
    files <- dplyr::filter(files, stringr::str_detect(.data$fullname, max(iters_available)))
  } else {
    if (!iteration %in% iters_available) {
      stop(sprintf('cannot find iteration %s', iteration))
    }
    files <- dplyr::filter(files, stringr::str_detect(.data$fullname, iteration))
  }

  if (varname == "all") {
    return(files)
  } else {
    if (varname == "") {
      files <- files[1L, ]
    } else {
      files <- dplyr::filter(files, stringr::str_detect(.data$fullname, varname))
    }
  }


  if (nrow(files) < 1)
    stop("no files found")

  tibble::tibble(fullname = files$fullname[1], date = ISOdatetime(2013, 01, 30, 0, 0, 0, tz = "UTC"))
}
