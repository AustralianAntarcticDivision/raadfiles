

#' @importFrom tibble tibble
.onLoad <- function(libname, pkgname) {
  run_on_load <- getOption("raadfiles.file.cache.disable")
  if (isTRUE(run_on_load)) {
    message("raadfiles in admin-mode, no file list loaded")
    return(invisible())
  }
  ## this logic says "data roots list is >=1 and I've set the file list/s found to the in-mem cache
  raad_path_was_set <- set_raad_data_roots(use_known_candidates = TRUE, replace_existing = FALSE)
  if (raad_path_was_set) {
    set_raad_filenames(clobber = TRUE)  ## clobber at start-up, why not
  } else {
    warning("no existing file cache found")
  }
  invisible()
}




