#'
#' Each file contains four time steps at six hourly intervals aligned to the
#' file base date.
#'
#' @references oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/8Day/4km/par
#'
#
#' @return tibble data frame of file names, with columns `fullname` and `date`
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @param time.resolution time resolution (only "8D" is available)
#' @name par
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' par_files()
par_files <- function(time.resolution = "8D"){
  files <- .find_files_generic(c("oceandata.sci.gsfc.nasa.gov", "MODISA/Mapped", "4km/par.*nc$"),
                               basefile_pattern =   if (nzchar(time.resolution)) sprintf("\\.%s\\.", time.resolution) else "")
if (nrow(files) < 1)
    stop("no files found")
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(substr(stringr::str_extract(basename(.data$fullname),
                                                                                      "[0-9]{8}"),1,8), "%Y%m%d"), tz = "UTC"))
  files <- dplyr::filter(files, !is.na(.data$date))
  files <- files[nrow(files):1, ]
  files <- dplyr::arrange(dplyr::distinct(files, .data$date,
                                          .keep_all = TRUE), date) %>% dplyr::select(.data$date,
                                                                                     .data$fullname, .data$root) %>% raadfiles:::set_dt_utc()

  files
}
