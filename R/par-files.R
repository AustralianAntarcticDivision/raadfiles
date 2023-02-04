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
#' @name par
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples

par_files <- function(time.resolution = "8daily"){
  files <- get_raad_filenames()
  files <- dplyr::filter(files, stringr::str_detect(.data$file,
                                                    "oceandata.sci.gsfc.nasa.gov"))
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file,
                                                                   "MODISA/Mapped/8Day/4km/par"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file,
                                                    ".*\\.nc$"))
  files <- dplyr::transmute(files, fullname = file.path(.data$root,
                                                        .data$file), .data$root)
  if (nrow(files) < 1)
    stop("no files found")
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(substr(stringr::str_extract(basename(.data$fullname),
                                                                                      "[0-9]{8}"),1,8), "%Y%m%d"), tz = "UTC")+4*60*60*24)
  files <- dplyr::filter(files, !is.na(.data$date))
  files <- files[nrow(files):1, ]
  files <- dplyr::arrange(dplyr::distinct(files, .data$date,
                                          .keep_all = TRUE), date) %>% dplyr::select(.data$date,
                                                                                     .data$fullname, .data$root) %>% raadfiles:::set_dt_utc()

  files <- dplyr::slice(files, rep(seq_len(nrow(files)), each = 4L))
  files[["date"]] <- files[["date"]] + rep(c(0, 6, 12, 15),
                                           length.out = nrow(files)) * 3600
  files
}
