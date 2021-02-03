sose_iters <- function() {
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "sose.ucsd.edu"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, ".*sose.ucsd.edu.*monthly.*nc$"))
  unique(stringr::str_extract(files$file, "ITER[0-9][0-9][0-9]"))
}

sose_vars <- function() {
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "sose.ucsd.edu"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, ".*sose.ucsd.edu.*monthly.*nc$"))
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
#' sose_monthly_files()
sose_monthly_files <- function(varname = "", iteration = "") {
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

  if (varname == "all") {
    return(files)
  } else {
    if (varname == "") {
      files <- files[1L, ]
    } else {
      files <- dplyr::filter(files, stringr::str_detect(fullname, varname))
    }
  }


  if (nrow(files) < 1)
    stop("no files found")

  ## expand file  list by dates in file
  nc <- RNetCDF::open.nc(files$fullname[1L])
  dates <- RNetCDF::var.get.nc(nc, "time")
  unit <- RNetCDF::att.get.nc(nc, "time", "units")
  cal <- RNetCDF::utcal.nc(unit, dates)
  dates <- ISOdatetime(cal[,1], cal[,2], cal[,3], cal[,4], cal[,5], cal[,6])
  tibble::tibble(fullname = files$fullname[1], date = dates, band = 1:length(dates))
}
