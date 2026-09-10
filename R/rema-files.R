## filter the PGC REMA collection by a further pattern, via the memoised search
.rema_pgc_filter <- function(pat) {
  .find_files_generic(c("data.pgc.umn.edu", pat))
}
#' @name rema_8m_files
#' @export
rema_tile_files <- function(all = FALSE, ...) {
  pat <- if (all) "Tile_Index_" else "Tile_Index_Rel1.shp$"
  .rema_pgc_filter(pat)
}
#' @name rema_8m_files
#' @export
rema_100m_files <- function( ...) {
  .rema_pgc_filter("v1.1.*100m_dem")
}
#' @name rema_8m_files
#' @export
rema_200m_files <- function(filled = TRUE, ...) {
  .rema_pgc_filter(if (filled) "v1.1.*200m_dem_filled" else "v1.1.*200m_dem.tif")
}
#' @name rema_8m_files
#' @export
rema_1km_files <- function(filled = TRUE, ...) {
  .rema_pgc_filter(if (filled) "v1.1.*1km_.*filled" else "v1.1.*1km_dem.tif")
}

#' Files for The Reference Elevation Model of Antarctica (REMA)
#'
#' Return files for various products from REMA Release 1
#'
#' 'rema_8m_files' returns the base level 8 GeoTIFF files, there are 1516 files at 8m resolution.
#' @references \url{https://www.pgc.umn.edu/tag/rema/}
#' @param filled return 'filled' variant if available
#' @param all for \code{rema_tile_files}, return all or just \code{*.shp} files; for other functions, if `TRUE` include 'data_deprecated', expert-use only
#' @param x pattern to detect
#' @param ... additional parameters, currently ignored
#'
#' @return data frame of file names
#' @export
#' @aliases rema_100m_files rema_200m_files rema_1km_files rema_tile_files
#' @examples
#' \dontrun{
#'   rema_8m_files()
#'   rema_100m_files(filled = TRUE)
#' }
rema_8m_files <- function(...) {
  pat <- "v1.1.*8m_dem.tif$"

  files <- .rema_pgc_filter(pat)
  if (nrow(files) < 1516) warning(sprintf("Only a subsample (%i) of the total (1516) 8m mosaic tiles is available. ", nrow(files)))

  .name_tiles(files)
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
