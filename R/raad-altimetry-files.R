#' Derived altimetry products
#'
#' A polar-transformed copy of the 'u' and 'v' components of surface currents from [altimetry_daily_files].
#' Only available for the southern hemisphere.
#'
#' The code that creates these derived files is at [raad-deriv](https://github.com/AustralianAntarcticDivision/raad-deriv).
#' @param hemisphere south only for now
#' @export
#' @examples
#' \dontrun{
#'   altimetry_currents_polar_files()
#' }
altimetry_currents_polar_files <- function(hemisphere = "south") {
  files <- dplyr::filter(get_raad_filenames(all = TRUE), stringr::str_detect(.data$file, "aad.gov.au/currents/polar"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, "grd$"))  ## faster without the .

  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), root = .data$root,
                              date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                        "%Y%m%d"),tz = "GMT"))

  if (nrow(files) < 1)
    stop("no files found")

  files <- dplyr::transmute(files, date = .data$date, fullname= .data$fullname, root = .data$root)
  bad <- is.na(files$date)

  ff <- split(files, grepl("polar_v", files$fullname))
  files <- ff[[1]] %>% dplyr::rename(ufullname = .data$fullname) %>% dplyr::inner_join(ff[[2]] %>%
                                                                                   dplyr::rename(vfullname = .data$fullname), "date") %>%
    dplyr::select(.data$date, .data$ufullname, .data$vfullname)

  dplyr::arrange(dplyr::distinct(files, .data$date, .keep_all = TRUE), .data$date)   %>%
    set_dt_utc()
}



#' Altimetry products
#'
#'
#' Sea Surface Height measured by Altimetry and derived variables. SSALTO/DUACS Near-Real-Time Level-4
#' sea surface height and derived variables measured by multi-satellite altimetry observations over
#' Global Ocean.
#'
#' In 2018/2019 the file servers migrated to 'my.cmems-du.au' and 'nrt.cmems-du.eu' (NRT) from 'ftp.sltac.cls.fr', but
#' the files and file name scheme remained unchanged so no net effect (so far that we are aware of).
#'
#' There are NRT (near-real-time) and final processed files, identifiable from the root domain and in the filename '^nrt_'. Both
#' are returned by this function. The `all` argument can be set to `TRUE` to include all NRT files, which has overlapping processing dates
#' that are ultimately consolidated into the daily sequence.
#'
#' @param all return all files or only the final processing (NRT is included either way)
#'
#' @return tibble data frame of file names, data 'date', and 'processing_date'
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name altimetry
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' \dontrun{
#'   altimetry_daily_files()
#' }
altimetry_daily_files <- function(all = FALSE) {
  ##  SSH etc now come from Copernicus, in a different format. Files
  ##  reside under
  ##  ftp.sltac.cls.fr/Core/SEALEVEL_GLO_PHY_L4_NRT_OBSERVATIONS_008_046
  ##  for near-real-time data and under
  ##  ftp.sltac.cls.fr/Core/SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047
  ##  for reprocessed data.
  ##  Each nc file now contains all the variables for that day (sla, mdt, uv)
  ## (The new collection is syncing now - 23-Jun-2017)

  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "SEALEVEL_GLO_PHY_L4"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, "nc$"))  ## faster without the .

  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), root = .data$root)
  dates <- stringr::str_extract(basename(files$fullname), "[0-9]{8}_[0-9]{8}")

  files$date <- as.POSIXct(as.Date(stringr::str_extract(dates, "[0-9]{8}"),"%Y%m%d"),tz = "UTC")
  files$processing_date <- as.POSIXct(as.Date(stringr::str_extract(unlist(lapply(strsplit(dates, "_"), "[", 2L)),
                                                                   "[0-9]{8}"),"%Y%m%d"),tz = "UTC")



  if (nrow(files) < 1)
    stop("no files found")

  files <- dplyr::transmute(files, date = .data$date, fullname= .data$fullname, root = .data$root, .data$processing_date)
  bad <- is.na(files$date)
  if (sum(bad) == 1 && which(bad) == nrow(files)) {
    files$date[bad] <- max(files$date, na.rm = TRUE) + 24 * 3600
  } else {
    files <- files[!is.na(files$date), ]
  }
  if (!all) {
    files <- dplyr::arrange(dplyr::distinct(files, .data$date, .keep_all = TRUE), .data$date)
  } else {
    files <- dplyr::arrange(files, .data$date, .data$processing_date)
  }

 files
}
