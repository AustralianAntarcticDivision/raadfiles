
#' CMIP5 files
#'
#' Currently in dev-test mode, using a very small subset until we get specifications.
#' @return
#' @export
#'
#' @examples
cmip5_files <-
  function() {
    files <- raadfiles:::get_raw_cfa_filenames()
    files <-  dplyr::filter(files,
                           stringr::str_detect(.data$file, "ccam/C96-5k_ACCESS1-0_rcp85/200705"))



  files <- dplyr::filter(files, stringr::str_detect(.data$file, "nc.*"))

    files <-   dplyr::transmute(files, file = .data$file, fullname = file.path(.data$root, .data$file))
    if (nrow(files) < 1)
      stop("no files found")
    #files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{6}.nc"),
    #                                                        "%Y%m"),tz = "GMT"))
    #dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
  files
  }

#iles <- cmip5_files()
#library(tidync)
#tidync(files$fullname[1])
