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
#' @return data frame with `fullname` file path, `x`, `y` tile column and row indices,  `root` the data file root path
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' \dontrun{
#'   srtm_files()
#' }
srtm_files <- function() {
pattern <- c("srtm.csi.cgiar.org", ".*\\.tif$", "srtm_5x5/TIFF/srtm")
  files <- .find_files_generic(pattern)
  if (nrow(files) < 1)
    stop("no smap files found")
  ## datadir <- get_raad_datadir()
  tile <- do.call(rbind, lapply(strsplit(basename(files$fullname), "_"), function(bs) as.integer(gsub("\\.tif$", "", bs[c(2, 3)]))))
  files[c("x", "y")] <- tile

  dplyr::select(files,  .data$fullname, .data$x, .data$y, .data$root)
}
