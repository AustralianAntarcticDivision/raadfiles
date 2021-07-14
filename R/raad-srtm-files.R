#' SRTM files
#'
#' SRTM 90m Digital Elevation Database v4.1
#'
#' DOI: 0.1080/13658810601169899
#' @references [https://cgiarcsi.community/data/srtm-90m-digital-elevation-database-v4-1/]
#' @return tibble data frame of file names, with columns `fullname`, `x`, `y` (tiles)
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name srtm
#' @aliases srtm
#' @return data frame with `fullname` file path, `x`, `y` tile column and row indices, `lon`, `lat`
#' a longitude and latitude for each tile, `root` the data file root path
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' \dontrun{
#'   srtm_files()
#' }
srtm_files <- function() {
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "srtm.csi.cgiar.org"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, ".*\\.tif$"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, "srtm_5x5/TIFF/srtm"))
  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), .data$root)

  if (nrow(files) < 1)
    stop("no smap files found")
  ## datadir <- get_raad_datadir()
  tile <- do.call(rbind, lapply(strsplit(basename(files$fullname), "_"), function(bs) as.integer(gsub("\\.tif$", "", bs[c(2, 3)]))))
  files[c("x", "y")] <- tile

  ## lon lat values
  r <- raster::raster(raster::extent(-180, 180, -60, 60), res = 5)
  files[c("lon", "lat")] <- raster::xyFromCell(r, raster::cellFromRowCol(r, files$y, files$x))
  dplyr::select(files,  .data$fullname, .data$x, .data$y, .data$lon, .data$lat, .data$root)
}
