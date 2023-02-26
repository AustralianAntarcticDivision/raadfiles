#' tokenize_oc <- function(x) {
#'   x <- stringr::str_split(fs::path_file (cfiles), "\\.", simplify = TRUE)
#'
#'   ## we might have type (GAC vs EFR) or NRT  but worry about that later
#'   ## suite is CHL product is chlor_a
#'   colnames(x) <- c("mission_sensor", "time", "level", "period", "suite", "product", "spatial")
#'   tibble::as_tibble(x)
#' }
#'
#' ocean_colour_files <- function(temporal = c("", "DAY", "8D", "MO", "R32"),
#'                                 spatial = c("", "9km", "4km"),
#'                                product = c("^[[:alpha:]]{2}", "AQUA_MODIS", "SEASTAR_SEAWIFS", "SNPP_VIIRS"),
#'                                varname = c("", "CHL", "PAR", "POC", "KD490"),
#'                                type = c("", "L3m", "L3b"),
#'                                bz2.rm = TRUE,
#'                                ext = c("", "nc", "main"),
#'                                ..., silent = FALSE) {
#'
#'   ## product is [mission]_[instrument] https://oceancolor.gsfc.nasa.gov/docs/filenaming-convention/
#'   #'AQUA_MODIS.20121210_20121217.L3m.8D.PAR.par.4km.nc'
#'   ftx <- dplyr::transmute(raadtools:::allfiles() |>
#'                             dplyr::filter(stringr::str_detect(file, "oceandata.sci.gsfc.nasa.gov")),
#'                           fullname = fs::path(root, file))$fullname
#'   # temporal <- match.arg(temporal)
#'   # product <- match.arg(product)
#'   # spatial <- match.arg(spatial)
#'   # ext <- match.arg(ext)
#'   temporal <- temporal[1]
#'   product <- product[1]
#'   spatial <- spatial[1]
#'   varname <- varname[1L]
#'   ext <- ext[1]
#'   type <- type[1]
#'   if (nzchar(ext)) ext <- sprintf("\\.%s", ext)
#'   ## don't forget those ST93c files!
#'   ## see here: http://oceancolor.gsfc.nasa.gov/DOCS/FormatChange.html
#'   #Note: ST92 is the test set designation for the SeaWiFS test run, similarly
#'   #AT108 and AT109 are the MODIS-Aqua test set designators.  These designations
#'   # will NOT be part of the reprocessing filenames.
#'   if (!nzchar(spatial)) spatial <- NULL
#'   if (!nzchar(varname)) varname <- NULL
#'   if (!nzchar(temporal)) temporal <- NULL
#'   if (!nzchar(type)) type <- NULL
#'   if (!nzchar(product)) product <- NULL
#'
#'   mtag <- sprintf(paste0("%s.*", ext), paste(c(product, type, temporal, varname, spatial), collapse = ".*"))
#'   if (!silent) print(mtag)
#'   #print(mtag)
#'   ##cfiles1 <- sapply(product, function(x) file.path("oceandata.sci.gsfc.nasa.gov", x)
#'   idx <- grep(mtag, fs::path_file(ftx))
#'   ftx <- ftx[idx]
#'   dates <- as.POSIXct(as.Date(stringr::str_extract(fs::path_file(ftx), "[0-9]{8}"), "%Y%m%d"),tz = "UTC")
#'
#'   if (length(ftx) < 1) stop("no files found for ", paste(c(product, type, temporal, varname, spatial), collapse = ", "))
#'
#'   tibble::tibble(fullname = ftx, date = dates)[order(dates), ]
#' }
