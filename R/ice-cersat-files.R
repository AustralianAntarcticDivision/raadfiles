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
 pattern <- c("ifremer",  "cersat.*daily/.*\\.nc$")
 files <- .find_files_generic(pattern)
 files <- files %>% mutate(date = as.POSIXct(as.Date(stringr::str_extract(.data$fullname, "[0-9]{8}"), "%Y%m%d"), tz = "UTC"))
 files %>% arrange(.data$date) %>% distinct(.data$date, .keep_all = TRUE)
}
