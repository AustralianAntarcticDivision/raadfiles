.rema_all_files <- function(all = FALSE, ...) {
  files <- dplyr::filter(get_raw_raad_filenames(), stringr::str_detect(.data$file,
                                                                       "data.pgc.umn.edu"))
  if (!all) files <- dplyr::filter(files, grepl("tif$", .data$file))
  files <- dplyr::transmute(files, file = .data$file, fullname = file.path(.data$root,
                                                                           .data$file))

  datadir <- get_raad_datadir()
  dplyr::mutate(files, file = stringr::str_replace(.data$fullname,  paste0(datadir, "/"), ""))
}
#' @name rema_8m_files
#' @export
rema_100m_files <- function(filled = TRUE, ...) {
  pat <- if (filled) "100m_.*filled" else "100m_"
  files <- .rema_all_files() %>% dplyr::filter(stringr::str_detect(.data$file, pat))
  if (nrow(files) < 1)
    stop("no files found")
  files
}
#' @name rema_8m_files
#' @export
rema_200m_files <- function() {
  pat <- if (filled) "200m_.*filled" else "200m_"
  files <- .rema_all_files() %>% dplyr::filter(stringr::str_detect(.data$file, "200m"))
  if (nrow(files) < 1)
    stop("no files found")
  files
}
#' @name rema_8m_files
#' @export
rema_1km_files <- function() {
  pat <- if (filled) "1km_.*filled" else "1km_"
  files <- .rema_all_files() %>% dplyr::filter(stringr::str_detect(.data$file, "1km"))
  if (nrow(files) < 1)
    stop("no files found")
  files
}

#' Files for The Reference Elevation Model of Antarctica (REMA)
#'
#' Return files for various products from REMA Release 1
#'
#' 'rema_8m_files' returns the base level 8 GeoTIFF files, there are 1254 files at 8m resolution.
#' @references \url{https://www.pgc.umn.edu/tag/rema/}
#' @param filled return 'filled' variant if available
#' @param ...
#'
#' @return data frame of file names
#' @export
#' @aliases rema_100m_files rema_200m_files rema_1km_files
#' @examples
#' rema_8m_files()
#' rema_100m_files(filled = TRUE)
rema_8m_files <- function(...) {
  pat <- "8m_dem.tif$"
  files <- .rema_all_files() %>% dplyr::filter(stringr::str_detect(.data$file, pat))
  if (nrow(files) < 1)
    stop("no files found")
  files
}
