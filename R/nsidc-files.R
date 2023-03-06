#' NSIDC daily and monthly sea-ice concentration
#'
#' Sea ice concentration files.
#'
#' @param extra_pattern argument for restricted string matching
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
  files <- nsidc_monthly_files_v2("PS_S")
   # files <- dplyr::filter(nsidc_monthly_files(), stringr::str_detect(.data$fullname, "_s\\.bin$"))
    ## arrange and distinct to resolve versions
    #files <- dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(.data$fullname)), .data$date, .keep_all = TRUE), .data$date)
    if (nrow(files) < 1)
        stop("no files found")
    files
}
#' @name nsidc
#' @export
nsidc_north_monthly_files <- function() {
  files <- nsidc_monthly_files_v2("PS_N")
    #files <- dplyr::filter(nsidc_monthly_files(), stringr::str_detect(.data$fullname, "_n\\.bin$"))
    ## arrange and distinct to resolve versions
    #files <- dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(.data$fullname)), .data$date, .keep_all = TRUE), .data$date)
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
  files <- nsidc_daily_files_v2("PS_S")
   # files <- dplyr::filter(nsidc_daily_files(), stringr::str_detect(.data$fullname, "_s\\.bin$"))
    ## arrange and distinct to resolve versions
    #files <- dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(.data$fullname)), .data$date, .keep_all = TRUE), .data$date)
    if (nrow(files) < 1)
        stop("no files found")
  files <- dplyr::distinct(files, date, .keep_all = TRUE)

    dplyr::arrange(files, date)
}
#' @name nsidc
#' @export
nsidc_north_daily_files <- function() {
  files <- nsidc_daily_files_v2("PS_N")
    #files <- dplyr::filter(nsidc_daily_files(), stringr::str_detect(fullname, "_n\\.bin$"))
    ## arrange and distinct to resolve versions
    #files <- dplyr::arrange(dplyr::distinct(dplyr::arrange(files, dplyr::desc(.data$fullname)), .data$date, .keep_all = TRUE), .data$date)
    if (nrow(files) < 1)
        stop("no files found")
   files <- dplyr::distinct(files, date, .keep_all = TRUE)
   dplyr::arrange(files, date)

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
    nrt_files <- .find_files_generic("nsidc\\.org", "nsidc0081_nrt_nasateam_seaice[/\\\\].*_f18_nrt_.*\\.bin$")

    dplyr::transmute(dplyr::bind_rows(final_files, nrt_files),
                     date = as.POSIXct(as.Date(stringr::str_sub(basename(.data$fullname), 4, 11), "%Y%m%d"), tz = "UTC"), .data$fullname)
}



#' @export
#' @name nsidc
nsidc_daily_files_v2 <- function(extra_pattern = NULL) {


  ## daily files come from final gsfc data, or from near-real-time data
  ## FINAL daily files are kept in the NSIDC-0051.001/YYYY.MM.DD folder, with filename nt_YYYYMMDD_*.bin
  ## note that top data folder NSIDC-0051.001 might change when the data version is incremented?
  #pattern <- c("n5eil01u\\.ecs\\.nsidc\\.org", "nsidc-0051\\.001[/\\\\].*[/\\\\]nt_[[:digit:]]{8}_.*\\.bin$")
  pattern <- c("nsidc\\.org", "NSIDC-0051\\.002", "nc$", extra_pattern)

  final_files <- .find_files_generic(pattern)

  pattern2 <- c("nsidc\\.org", "NSIDC-0081\\.002", ".*nc$", extra_pattern)
  ## near-real-time files
  #nrt_files <- .find_files_generic("nsidc0081_nrt_nasateam_seaice[/\\\\].*_f18_nrt_.*\\.bin$")
  nrt_files <- .find_files_generic(pattern2)
  out <- dplyr::transmute(dplyr::bind_rows(final_files, nrt_files), date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"), "%Y%m%d"), tz = "UTC"), .data$fullname)
  out <- dplyr::filter(out, !is.na(.data$date))

  dplyr::arrange(out, .data$date)
}



#' @export
#' @name nsidc
nsidc_monthly_files_v2 <- function(extra_pattern = NULL) {


  ## daily files come from final gsfc data, or from near-real-time data
  ## FINAL daily files are kept in the NSIDC-0051.001/YYYY.MM.DD folder, with filename nt_YYYYMMDD_*.bin
  ## note that top data folder NSIDC-0051.001 might change when the data version is incremented?
  #pattern <- c("n5eil01u\\.ecs\\.nsidc\\.org", "nsidc-0051\\.001[/\\\\].*[/\\\\]nt_[[:digit:]]{8}_.*\\.bin$")
  pattern <- c("nsidc\\.org", "NSIDC-0051\\.002", "nc$", extra_pattern)

  final_files <- .find_files_generic(pattern)

  pattern2 <- c("nsidc\\.org", "NSIDC-0081\\.002", ".*nc$", extra_pattern)
  ## near-real-time files
  #nrt_files <- .find_files_generic("nsidc0081_nrt_nasateam_seaice[/\\\\].*_f18_nrt_.*\\.bin$")
  nrt_files <- .find_files_generic(pattern2)
  files <- dplyr::bind_rows(final_files, nrt_files)
  files  <- files[stringr::str_length(basename(files$fullname)) < 42, ]
  yyyymm <- stringr::str_extract(basename(files$fullname), "[0-9]{6}")
  #files <- files[!is.na(yyyymm), ]; yyyymm <- yyyymm[!is.na(yyyymm)]
  out <- dplyr::transmute(files, date = as.POSIXct(as.Date(sprintf("%s01", yyyymm), "%Y%m%d"), tz = "UTC"), .data$fullname)
  out <- dplyr::filter(out, !is.na(.data$date))
  dplyr::arrange(out, .data$date)
}
