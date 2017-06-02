#' @importFrom tibble tibble
.onLoad <- function(libname, pkgname) {
  op <- options()
  raadfiles.default.data.directory <- "/rdsi/PRIVATE/raad"
  fs <- NULL
  load(  file.path(raadfiles.default.data.directory, "admin", "filelist", "allfiles2.Rdata"))
  op.raadfiles <- list(
    raadfiles.default.data.directory = raadfiles.default.data.directory,
    raadfiles.filename.database = tibble::tibble(fullname = file.path(raadfiles.default.data.directory, fs))
    )
  toset <- !(names(op.raadfiles) %in% names(op))
  if(any(toset)) options(op.raadfiles[toset])

  invisible()
}
