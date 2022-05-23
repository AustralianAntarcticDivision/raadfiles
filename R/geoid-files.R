.geoid_all_files <- function(all = FALSE, ...) {
  # earth-info.nga.mil/GandG/wgs84/gravitymod/egm2008/GIS/world_geoid
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file,
                                                                   "earth-info.nga.mil"))

  if (!all) files <- dplyr::filter(files, grepl("w001001\\.adf$", .data$file))
  files <- dplyr::transmute(files,fullname = file.path(.data$root, .data$file), root = .data$root)
  files
}

#' Earth Gravitation Model files
#'
#' Global 2.5 Minute Geoid Undulations, a
#'
#' Each file is an ESRI GRID raster data set of 2.5-minute geoid undulation values covering a 45 x 45 degree area.
#' Each raster file has a 2.5-minute cell size and is a subset of the global 2.5 x 2.5-minute grid of pre-computed
#' geoid undulation point values found on the EGM2008-WGS 84 Version web page. This ESRI GRID format represents a
#' continuous surface of geoid undulation values where each 2.5-minute raster cell derives its value from the original
#' pre-computed geoid undulation point value located at the SW corner of each cell.
#' @param all return all files, or just the core grid files for GDAL? './w001001.adf'
#' @param ... : additional parameters, currently ignored
#' @name geoid_files
#' @return data frame of file paths
#' @export
#' @examples
#' \dontrun{
#'   geoid_files()
#' }
geoid_files <- function(all = FALSE, ...) {
 .geoid_all_files(all = FALSE)
}
