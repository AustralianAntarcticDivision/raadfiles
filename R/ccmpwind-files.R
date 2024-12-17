#' Derived surface files from Remote Sensing Systems (RSS)
#'
#' RSS CCMP_RT V2.1 derived surface winds (Level 3.0), variables 'uwnd', 'vwnd', 'nobs'
#' 'u-wind vector component at 10 meters', 'v-wind vector component at 10 meters', and
#' 'number of observations used to derive wind vector components' from [Remote Sensing Systems](http://www.remss.com/).
#'
#' Each file contains four time steps at six hourly intervals aligned to the
#' file base date.
#'
#' @references "Mears et al., Journal of Geophysical Research: Oceans,124,
#'   6997-7010, 2019, Hoffman et al., Journal of Atmospheric and Oceanic
#'   Technology, 2013; Atlas et al., BAMS, 2011; Atlas et al., BAMS, 1996". "Available for
#'   public use with proper citation".
#'
#
#' @return tibble data frame of file names, with columns `fullname` and `date`
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name ccmp
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @examples
#' \dontrun{
#'   ccmp_6hourly_files()
#' }
ccmp_6hourly_files <- function() {
  #ftp://podaac-ftp.jpl.nasa.gov/allData/smap/L3/RSS/V3/8day_running/SCI/40KM/2015/272/

  pattern <- c("remss.com", "ccmp/.*CCMP.*_Wind_Analysis_[0-9]", ".*\\.nc$")
  files <- .find_files_generic(pattern)
  if (nrow(files) < 1)
    stop("no ccmp files found")
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "UTC"))

  files <- dplyr::filter(files, !is.na(.data$date))

  ## put all NRT last before distinct by date
  files <- files[nrow(files):1, ]
files <- dplyr::arrange(dplyr::distinct(files, .data$date, .keep_all = TRUE), desc(fullname), date)  %>%
    dplyr::select(.data$date, .data$fullname, .data$root) |> dplyr::arrange(date)

  files
}
