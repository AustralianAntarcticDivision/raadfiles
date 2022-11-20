#' NSIDC daily and monthly sea-ice concentration
#'
#' Sea ice concentration files.
#'
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name nsidc
#' @export
#' @importFrom stringr str_detect str_extract str_replace
#' @export
#' @examples
#' \dontrun{
#'   nsidc_south_monthly_files()
#'   nsidc_north_monthly_files()
#'   nsidc_monthly_files()
#'   nsidc_south_daily_files()
#'   nsidc_north_daily_files()
#'   nsidc_daily_files()
#' }
nsidc_south_monthly_files <- function() {
    files <- dplyr::filter(nsidc_monthly_files(), stringr::str_detect(.data$fullname, "_s\\.bin$"))
    ## arrange and distinct to resolve versions
    files <- dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(.data$fullname)), .data$date, .keep_all = TRUE), .data$date)
    if (nrow(files) < 1)
        stop("no files found")
    files
}
#' @name nsidc
#' @export
nsidc_north_monthly_files <- function() {
    files <- dplyr::filter(nsidc_monthly_files(), stringr::str_detect(.data$fullname, "_n\\.bin$"))
    ## arrange and distinct to resolve versions
    files <- dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(.data$fullname)), .data$date, .keep_all = TRUE), .data$date)
    if (nrow(files) < 1)
        stop("no files found")
    files
}
#' @name nsidc
#' @export
nsidc_monthly_files <- function() {
    pattern <- c("n5eil01u\\.ecs\\.nsidc\\.org", "NSIDC-0051\\.001[/\\\\][[:digit:]]{4}\\.[[:digit:]]{2}\\.01[/\\\\]nt_[[:digit:]]{6}_.*\\.bin$")
    files <- .find_files_generic(pattern)
    files <- dplyr::transmute(files, date = as.POSIXct(as.Date(sprintf("%s01", stringr::str_sub(basename(.data$fullname), 4, 9)), "%Y%m%d"), tz = "UTC"),
                              .data$fullname, .data$root)
    files

}
#' @name nsidc
#' @export
nsidc_south_daily_files <- function() {
    files <- dplyr::filter(nsidc_daily_files(), stringr::str_detect(.data$fullname, "_s\\.bin$"))
    ## arrange and distinct to resolve versions
    files <- dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(.data$fullname)), .data$date, .keep_all = TRUE), .data$date)
    if (nrow(files) < 1)
        stop("no files found")
    files
}
#' @name nsidc
#' @export
nsidc_north_daily_files <- function() {
    files <- dplyr::filter(nsidc_daily_files(), stringr::str_detect(fullname, "_n\\.bin$"))
    ## arrange and distinct to resolve versions
    files <- dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(.data$fullname)), .data$date, .keep_all = TRUE), .data$date)
    if (nrow(files) < 1)
        stop("no files found")
    files
}
#' @name nsidc
#' @export
nsidc_daily_files <- function() {


  ## daily files come from final gsfc data, or from near-real-time data
    ## FINAL daily files are kept in the NSIDC-0051.001/YYYY.MM.DD folder, with filename nt_YYYYMMDD_*.bin
    ## note that top data folder NSIDC-0051.001 might change when the data version is incremented?
    pattern <- c("n5eil01u\\.ecs\\.nsidc\\.org", "NSIDC-0051\\.001[/\\\\].*[/\\\\]nt_[[:digit:]]{8}_.*\\.bin$")
    final_files <- .find_files_generic(pattern)

    ## near-real-time files
    nrt_files <- .find_files_generic("nsidc0081_nrt_nasateam_seaice[/\\\\].*_f18_nrt_.*\\.bin$")

    dplyr::transmute(dplyr::bind_rows(final_files, nrt_files), date = as.POSIXct(as.Date(stringr::str_sub(basename(.data$fullname), 4, 11), "%Y%m%d"), tz = "UTC"), .data$fullname)
}
