#' Data frame of all available fast ice files.
#'
#' A data frame with file, date, fullname
#'
#' Note that this product changed from the legacy 2000-2008 initial East Antarctic product
#' "binary_fast_ice" to the circumpolar update "circum_fast_ice" 2000-2018 in Feburary 2021.
#'
#' If you want the old files, use `product = "binary_fast_ice"`, but it's safe to assume
#' the default product supersedes the old one.
#'
#' The initial product was in Cylindrical Equal Area projection, while the circumpolar product
#' uses the NSIDC-compatible polar stereographic (but with an unspecified extent, though implicit in the
#' longitude and latitude arrays of the NetCDF files).
#'
#' Exists in 'public.services.aad.gov.au/datasets/science' (Feb 2021).
#'
#' @references Fraser, A. D., Massom, R. A., Ohshima, K. I., Willmes, S.,
#'  Kappes, P. J., Cartwright, J., and Porter-Smith, R.:
#'  High-resolution mapping of circum-Antarctic landfast sea ice distribution,
#'  2000–2018, Earth Syst. Sci. Data, 12, 2987–2999, https://doi.org/10.5194/essd-12-2987-2020, 2020.
#'
#' [Fraser et al. 2018](https://doi.org/10.5194/essd-12-2987-2020)
#' @title fast ice files
#' @param product which product
#' @param mask if TRUE return mask file name
#' @param ... reserved for future use, currently ignored
#' @return data frame
#' @export
fasticefiles <- function(product = c("circum_fast_ice", "binary_fast_ice"), mask = FALSE, ...) {
  product <- match.arg(product)
  #pref <- file.path("fastice", "fraser_fastice", product)
  #fs <- list.files(file.path(datadir, pref), pattern = "img$")
  if (product == "binary_fast_ice") {

    pattern <- c("data\\.aad\\.gov\\.au", "file/3656")
    files <- .find_files_generic(pattern)
    if (mask) {
      files <- dplyr::filter(files, stringr::str_detect(.data$fullname, ".*geo/coastmask.img$"))
      return(files$fullname)
    }

    files <- dplyr::filter(files, stringr::str_detect(.data$fullname, "sqc.img$"))

    files$date <- as.POSIXct(strptime(basename(files$fullname), "%Y_%j"), tz = "UTC")
    return(files)
  }
  if (product == "circum_fast_ice") {

    pattern <- c("public\\.services\\.aad\\.gov\\.au", "AAS_4116_Fraser_fastice_circumantarctic.*nc$")
    files <- .find_files_generic(pattern)
    ## we must expand to the internal band/date

    on.exit(sink(NULL), add = TRUE)
    sink(tempfile())
    time <- lapply(files$fullname, function(x) raster::getZ(raster::brick(x)))
    files <- files[rep(seq_len(nrow(files)), lengths(time)), ]
    files$date <- as.POSIXct(as.Date("1970-01-01") + unlist(time), tz = "UTC")
    files$band <- unlist(lapply(lengths(time), seq_len))

    return(tibble::tibble(fullname = files$fullname,
                          date = files$date, band = files$band))
  }
}
