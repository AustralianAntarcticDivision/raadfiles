#' Altimetry products
#'
#'
#' Sea Surface Height measured by Altimetry and derived variables. SSALTO/DUACS Near-Real-Time Level-4
#' sea surface height and derived variables measured by multi-satellite altimetry observations over
#' Global Ocean.
#'
#' In 2018/2019 the file servers migrated to 'my.cmems-du.au' and 'nrt.cmems-du.eu' from 'ftp.sltac.cls.fr', but
#' the files and file name scheme remained unchanged so no net effect (so far that we are aware of).
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

  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "SEALEVEL_GLO_PHY_L4"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, "nc$"))  ## faster without the .

  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), root = .data$root,
                              date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                        "%Y%m%d"),tz = "GMT"))

  if (nrow(files) < 1)
    stop("no files found")

  files <- dplyr::transmute(files, date = .data$date, fullname= .data$fullname, root = .data$root)
  bad <- is.na(files$date)
  if (sum(bad) == 1 && which(bad) == nrow(files)) {
    files$date[bad] <- max(files$date, na.rm = TRUE) + 24 * 3600
  } else {
    files <- files[!is.na(files$date), ]
  }
  dplyr::arrange(dplyr::distinct(files, .data$date, .keep_all = TRUE), .data$date)   %>%
    set_dt_utc()
}
