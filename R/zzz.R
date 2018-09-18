

#' @importFrom tibble tibble
.onLoad <- function(libname, pkgname) {
  raad_path_was_set <- set_raadfile_data_roots(use_known_candidates = TRUE, replace_existing = FALSE)
  if (raad_path_was_set) {
    set_raw_raad_filenames()
  } else {
    warning("no existing file cache found")
  }
  invisible()
}




