#' Data frame of all available fast ice files.
#'
#' A data frame with file, date, fullname
#' @title fast ice files
#' @param product which product
#' @param mask if TRUE return mask file name
#' @param ... reserved for future use, currently ignored
#' @return data frame
#' @export
fasticefiles <- function(product = "binary_fast_ice", mask = FALSE, ...) {
  product <- match.arg(product)
  #pref <- file.path("fastice", "fraser_fastice", product)
  #fs <- list.files(file.path(datadir, pref), pattern = "img$")
  files <- dplyr::filter(get_raad_filenames(all = TRUE), stringr::str_detect(.data$file, "data.aad.gov.au"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, "file/3656"))


  if (mask) {
    files <- dplyr::filter(files, stringr::str_detect(.data$file, ".*geo/coastmask.img$"))
    return(file.path(files$root, files$file))
  }
  files <- dplyr::filter(files, stringr::str_detect(.data$file, "sqc.img$"))

  dates <- as.POSIXct(strptime(basename(files$file), "%Y_%j"), tz = "GMT")
  tibble::tibble(date = dates, fullname = file.path(files$root, files$file))

}
