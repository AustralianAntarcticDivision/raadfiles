#' Altimetry products
#'
#' From Copernicus
#'
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name altimetry
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' altimetry_daily_files()
altimetry_daily_files <- function() {
#  SSH etc now come from Copernicus, in a different format. Files
#  reside under
#  ftp.sltac.cls.fr/Core/SEALEVEL_GLO_PHY_L4_NRT_OBSERVATIONS_008_046
#  for near-real-time data and under
#  ftp.sltac.cls.fr/Core/SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047
#  for reprocessed data.
#  Each nc file now contains all the variables for that day (sla, mdt, uv)
# (The new collection is syncing now - 23-Jun-2017)

  files <- dplyr::filter(get_raw_raad_filenames(), stringr::str_detect(.data$file, "SEALEVEL_GLO_PHY_L4"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, "nc$"))  ## faster without the .

  files <-   dplyr::transmute(files, file = .data$file, fullname = file.path(.data$root, .data$file))

  if (nrow(files) < 1)
    stop("no files found")
  datadir <- get_raad_datadir()
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "GMT"),
                         file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""))
  ## hope for the best, this is nrt "latest"
  files$date[is.na(files$date)] <- max(files$date, na.rm = TRUE) + 24 * 3600
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
}
