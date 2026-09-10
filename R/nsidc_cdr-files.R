#' NSIDC daily and monthly sea-ice concentration
#'
#' Find NSIDC sea ice concentration files. Two families are supported:
#'
#' - `nsidc_*_files()`: NASA Team 25km polar stereographic files
#'   (NSIDC-0051 final and NSIDC-0081 near-real-time).
#' - `nsidc_cdr_*_files()`: NOAA/NSIDC Climate Data Record netCDF files
#'   (G02202).
#'
#' Each family has hemisphere-specific (`_south_`, `_north_`) and combined
#' variants. Hemisphere-specific variants return one file per date, resolving
#' duplicates (data versions, sensor platforms) by preferring the lexically
#' last file name. Combined variants return both hemispheres, ordered by date
#' then file name, so callers should not assume one row per date.
#'
#' @param extra_pattern optional character vector of extra regular expressions
#'   to further restrict matching (CDR functions only)
#' @param version CDR data version tag as it appears in the data path,
#'   default "V6"
#' @return tibble data frame with columns `date` (POSIXct, UTC) and `fullname`
#' @name nsidc
#' @examples
#' \dontrun{
#'   nsidc_south_monthly_files()
#'   nsidc_north_monthly_files()
#'   nsidc_monthly_files()
#'   nsidc_south_daily_files()
#'   nsidc_north_daily_files()
#'   nsidc_daily_files()
#'
#'   nsidc_cdr_south_daily_files()
#'   nsidc_cdr_north_daily_files()
#'   nsidc_cdr_daily_files()
#'   nsidc_cdr_south_monthly_files()
#'   nsidc_cdr_north_monthly_files()
#'   nsidc_cdr_monthly_files()
#' }
NULL

## ---------------------------------------------------------------------------
## internal helpers
## ---------------------------------------------------------------------------

.nsidc_check <- function(files) {
  if (nrow(files) < 1) stop("no files found")
  files
}

## one file per date: prefer the lexically last file name, which sorts higher
## data versions / later platform ids ahead of older ones
.nsidc_one_per_date <- function(files) {
  files <- dplyr::arrange(files, dplyr::desc(.data$fullname))
  files <- dplyr::distinct(files, .data$date, .keep_all = TRUE)
  dplyr::arrange(files, .data$date)
}

## combine hemispheres: stable order by date, then file name
.nsidc_combine <- function(...) {
  files <- dplyr::bind_rows(...)
  files[order(files$date, basename(files$fullname)), ]
}

## CDR (G02202) file finder shared by all nsidc_cdr_* functions
##
## paths look like
##  noaadata.apps.nsidc.org/NOAA/G02202_V6/north/daily/1978/sic_psn25_19781025_n07_v06r00.nc
##  noaadata.apps.nsidc.org/NOAA/G02202_V6/south/monthly/sic_pss25_197811_n07_v06r00.nc"
.nsidc_cdr_files <- function(time.resolution = c("daily", "monthly"),
                             hemisphere = c("both", "south", "north"),
                             version = "V6",
                             extra_pattern = NULL) {
  time.resolution <- match.arg(time.resolution)
  hemisphere <- match.arg(hemisphere)

  hemi_pattern <- switch(hemisphere,
                         both = NULL,
                         south = "_pss25",
                         north = "_psn25")
  pattern <- c("noaadata.apps.nsidc.org", sprintf("G02202_%s", version),
               hemi_pattern, time.resolution, "nc$", extra_pattern)
  files <- .nsidc_check(.find_files_generic(pattern))

  bn <- basename(files$fullname)
  datestr <- switch(time.resolution,
                    daily = stringr::str_extract(bn, "[0-9]{8}"),
                    monthly = sprintf("%s01", stringr::str_extract(bn, "[0-9]{6}")))

  out <- dplyr::transmute(files,
                          date = as.POSIXct(as.Date(datestr, "%Y%m%d"), tz = "UTC"),
                          .data$fullname, .data$root)
  out <- dplyr::filter(out, !is.na(.data$date))

  if (hemisphere == "both") {
    ## one file per date within each hemisphere
    south <- .nsidc_one_per_date(out[grepl("psn", basename(out$fullname)), ])
    north <- .nsidc_one_per_date(out[grepl("pss", basename(out$fullname)), ])
    out <- .nsidc_combine(north, south)
  } else {
    out <- .nsidc_one_per_date(out)
  }
  .nsidc_check(out)
}




## ---------------------------------------------------------------------------
## Climate Data Record (G02202)
## ---------------------------------------------------------------------------

#' @name nsidc
#' @export
nsidc_cdr_south_daily_files <- function(extra_pattern = NULL, version = "V6") {
  .nsidc_cdr_files("daily", "south", version = version, extra_pattern = extra_pattern)
}

#' @name nsidc
#' @export
nsidc_cdr_north_daily_files <- function(extra_pattern = NULL, version = "V6") {
  .nsidc_cdr_files("daily", "north", version = version, extra_pattern = extra_pattern)
}

#' @name nsidc
#' @export
nsidc_cdr_daily_files <- function(extra_pattern = NULL, version = "V6") {
  .nsidc_cdr_files("daily", "both", version = version, extra_pattern = extra_pattern)
}

#' @name nsidc
#' @export
nsidc_cdr_south_monthly_files <- function(extra_pattern = NULL, version = "V6") {
  .nsidc_cdr_files("monthly", "south", version = version, extra_pattern = extra_pattern)
}

#' @name nsidc
#' @export
nsidc_cdr_north_monthly_files <- function(extra_pattern = NULL, version = "V6") {
  .nsidc_cdr_files("monthly", "north", version = version, extra_pattern = extra_pattern)
}

#' @name nsidc
#' @export
nsidc_cdr_monthly_files <- function(extra_pattern = NULL, version = "V6") {
  .nsidc_cdr_files("monthly", "both", version = version, extra_pattern = extra_pattern)
}
