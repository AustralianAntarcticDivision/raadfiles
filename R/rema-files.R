.rema_all_files <- function(all = FALSE, ...) {
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file,
                                                                       "data.pgc.umn.edu"))
  if (!all) files <- dplyr::filter(files, grepl("tif$", .data$file))
  files <- dplyr::transmute(files,fullname = file.path(.data$root, .data$file), root = .data$root)
}
#' @name rema_8m_files
#' @export
rema_tile_files <- function(all = FALSE, ...) {
  pat <- if (all) "Tile_index_" else "Tile_Index_.*shp$"
  .rema_all_files(all = TRUE) %>% dplyr::filter(stringr::str_detect(.data$fullname, pat))
}
#' @name rema_8m_files
#' @export
rema_100m_files <- function(filled = TRUE, ...) {
  pat <- if (filled) "100m_.*filled" else "100m_"
  files <- .rema_all_files() %>% dplyr::filter(stringr::str_detect(.data$fullname, pat))

  if (nrow(files) < 1)
    stop("no files found")
  files
}
#' @name rema_8m_files
#' @export
rema_200m_files <- function() {
  pat <- "200m_"
  files <- .rema_all_files() %>% dplyr::filter(stringr::str_detect(.data$fullname, pat))
  if (nrow(files) < 1)
    stop("no files found")
  files
}
#' @name rema_8m_files
#' @export
rema_1km_files <- function(filled = TRUE, ...) {
  pat <- if (filled) "1km_.*filled" else "1km_"
  files <- .rema_all_files() %>% dplyr::filter(stringr::str_detect(.data$fullname, pat))
  if (nrow(files) < 1)
    stop("no files found")
  files
}

#' Files for The Reference Elevation Model of Antarctica (REMA)
#'
#' Return files for various products from REMA Release 1
#'
#' 'rema_8m_files' returns the base level 8 GeoTIFF files, there are 1516 files at 8m resolution.
#' @references \url{https://www.pgc.umn.edu/tag/rema/}
#' @param filled return 'filled' variant if available
#' @param ...
#'
#' @return data frame of file names
#' @export
#' @aliases rema_100m_files rema_200m_files rema_1km_files rema_tile_files
#' @examples
#' rema_8m_files()
#' rema_100m_files(filled = TRUE)
rema_8m_files <- function(...) {
  pat <- "8m_dem.tif$"
  files <- .rema_all_files() %>% dplyr::filter(stringr::str_detect(.data$fullname, pat))
  if (nrow(files) < 1)
    stop("no files found")
  if (nrow(files) < 1516) warning(sprintf("Only a subsample (%i) of the total (1516) 8m mosaic tiles is available. ", nrow(files)))

  files
}

.write_rema_vrt <- function(product = "dem_8m", clobber = FALSE) {
  product <- match.arg(product)
  files <- rema_8m_files()
  bname <- file.path(dirname(dirname(files$fullname[1])), "rema_mosaic_8m_dem.vrt")
  if (!clobber && file.exists(bname)) stop("file exists: \n'", bname, "'\n use 'clobber = TRUE' to overwrite")
  print(sprintf("Creating mosaic %s", bname))

  ## the file list is long so we have to go via text input_file
  tfile <- tempfile()
  err <- try(writeLines(files$fullname, tfile), silent = TRUE)
  if (inherits(err, "try-error")) stop("cannot create tempfile at ", tfile, "for VRT input list")

  ## we can't get past clobber = FALSE, so set overwrite here
  sys <- sprintf("gdalbuildvrt -overwrite -input_file_list %s %s", tfile, bname)
  system(sys, intern = TRUE)
}
