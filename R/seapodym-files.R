
#' SEAPODYM model files
#'
#' Global ocean low and mid trophic levels biomass hindcast
#' @references \url{http://www.cls.fr}, \url{http://www.seapodym.eu}
#' @export
seapodym_weekly_files <- function() {
  pattern <- "cmems-global-reanalysis-bio-001-033-weekly-extract"
  files <-   .find_files_generic(pattern)

  ## datadir <- get_raad_datadir()
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "UTC"))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)  %>%
    dplyr::select("date", "fullname", "root")

}
