#' @importFrom tibble tibble
.onLoad <- function(libname, pkgname) {
  op <- options()
  raadfiles.default.data.directory <- "/rdsi/PRIVATE/raad"
  fs <- NULL
  load(  file.path(raadfiles.default.data.directory, "admin", "filelist", "allfiles2.Rdata"))
  op.raadfiles <- list(
    raadfiles.default.data.directory = raadfiles.default.data.directory,
    raadfiles.filename.database = tibble::tibble(root = raadfiles.default.data.directory, file = fs)
    )
  toset <- !(names(op.raadfiles) %in% names(op))
  if(any(toset)) options(op.raadfiles[toset])
  ## try load ncdf4 now to avoid message and delay later
  a <- try(ncdf4::nc_version(), silent = TRUE)
  invisible()
}
