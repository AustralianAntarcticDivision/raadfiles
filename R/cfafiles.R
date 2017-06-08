#' BOM tmax daily fils
#'
#' Data is daily but arranged in monthly files.
#' @importFrom progress progress_bar
#' @return
#' @export
#'
#' @examples
bom_tmax_daily_files <- function() {
  files <- dplyr::filter(get_raw_cfa_filenames(), stringr::str_detect(.data$file, "BoM_daily_vars/tmax"))
  files <-   dplyr::filter(files, stringr::str_detect(.data$file, "tmax_day.*\\.nc$"))
  files <-   dplyr::transmute(files, file = .data$file, fullname = file.path(.data$root, .data$file))

  if (nrow(files) < 1)
    stop("no files found")


  #system.time(system(sprintf("ncks -m %s", ff), intern = TRUE))
#   library(microbenchmark)
#  microbenchmark::microbenchmark(
#    ncks_prs = system(sprintf("ncks -m %s | grep 'time size'", ff), intern = TRUE),
#   ncdump_prs = system(sprintf("ncdump -h %s | grep UNLIMITED", ff), intern = TRUE),
#   ncdump_str = as.integer(na.omit(str_match(system(sprintf("ncdump -h %s", ff), intern = TRUE), ".*UNLIMITED.*([0-9]{2}).*")[, 2] )),
#   RNetCDF = get_dim(ff, "time"),
#  times = 10
# )
  # system.time(files$n_slice <- unlist(lapply(files$fullname, get_dim, varname = "time")))
  # user  system elapsed
  # 5.968   1.948  33.520

  # library(future)
  # plan(multiprocess)
  # system.time(files$n_slice <- unlist(future_lapply(files$fullname, raadfiles:::get_dim, varname = "time")))

  pb <- progress::progress_bar$new(
    format = "  determining time dimension per file [:bar] :percent in :elapsed",
    total = nrow(files), clear = FALSE, width= 60)
  pb$tick(0)

  files$n_slice <- unlist(lapply(files$fullname, get_dim, varname = "time", progress_ticker = pb))
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "GMT"))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)
}

#' @importFrom RNetCDF dim.inq.nc open.nc close.nc
get_dim <- function(filename, varname, progress_ticker = NULL) {
  progress_ticker$tick()
  on.exit(RNetCDF::close.nc(nc))
  nc <- RNetCDF::open.nc(filename)
  RNetCDF::dim.inq.nc(nc, varname)$length
}

get_raw_cfa_filenames <- function() {
  getOption("cfafiles.filename.database" )
}
get_cfa_datadir <- function() {
  getOption("cfafiles.default.data.directory")
}
