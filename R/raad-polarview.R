#' Polarview files
#'
#' Imagery from www.polarview.aq
#'
#' The JPEGs are simple images, the GeoTIFFs are 16-bit integers (haven't explored further)
#' @return tibble data frame of file names, with columns `fullname` and `date`
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name polarview
#' @export
#' @param type jpeg or tarball
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' files <- polarview_files()
#' tiffiles <- polarview_files(type = "tarball")
#'
polarview_files <- function(type = c("jpeg", "tarball")) {
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "www.polarview.aq/images"))
  type <- match.arg(type)
  patt <- switch(type,
         tarball = "104_S1geotiff.*tif\\.tar\\.gz$",
         jpeg = "106_S1jpgsmall.*jpg$")
  files <- dplyr::filter(files,     grepl(patt, .data$file))

  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), .data$root)

  if (nrow(files) < 1)
    stop("no files found")
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "GMT"))

  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)  %>%
    dplyr::select(.data$date, .data$fullname, .data$root) %>%
    set_dt_utc()
}
