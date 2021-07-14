#' CERSAT daily  sea-ice concentration
#'
#' Sea ice concentration files at 12.5 km resolution, southern hemisphere.
#'
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate transmute
#' @importFrom rlang .data
#' @export
#' @importFrom stringr str_detect str_extract
#' @export
#' @examples
#' \dontrun{
#'   cersat_daily_files()
#' }
cersat_daily_files <- function() {
  files <- dplyr::filter(get_raad_filenames(),
                         stringr::str_detect(.data$file, "ifremer"))


  files <- dplyr::filter(files, stringr::str_detect(.data$file, "cersat.*daily/.*\\.nc$"))
 files <- files %>% mutate(date = as.POSIXct(as.Date(stringr::str_extract(.data$file, "[0-9]{8}"), "%Y%m%d"), tz = "GMT")) %>%
   dplyr::transmute(date = .data$date, fullname = file.path(.data$root, .data$file), root = .data$root)
 files %>% arrange(.data$date) %>% distinct(.data$date, .keep_all = TRUE)   %>%
   set_dt_utc()
}
