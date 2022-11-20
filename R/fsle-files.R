#' Backward-in-time Finite-Size Lyapunov Exponents.
#'
#' FSLE - MAPS OF FINITE SIZE LYAPUNOV EXPONENTS AND ORIENTATIONS OF THE ASSOCIATED EIGENVECTORS
#'
#' These are daily files.
#' @export
#' @references [https://www.aviso.altimetry.fr/en/data/products/value-added-products/fsle-finite-size-lyapunov-exponents.html]( Finite-Size Lyapunov Exponents)
fsle_files <- function() {

  pattern <- c("ftp-access.aviso", "lyapunov", ".*nc$")
  files <- .find_files_generic(pattern)

  if (nrow(files) < 1)
    stop("no files found")
  ## datadir <- get_raad_datadir()
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "GMT"))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)  %>%
    dplyr::select(.data$date, .data$fullname, .data$root) %>%
    set_dt_utc()
}

