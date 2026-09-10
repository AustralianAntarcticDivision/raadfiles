raadfiles.env <- new.env(FALSE, parent=globalenv())

env0 <- new.env(FALSE, parent=globalenv())
assign("message2", NULL, envir = env0)
assign("message1", NULL, envir = env0)
.onAttach <- function(libname, pkgname) {
  mess1 <- get("message1", envir = env0)
  if (!is.null(mess1)) {
    packageStartupMessage(mess1)
  }
  mess2 <- get("message2", envir = env0)
  if (!is.null(mess2)) {
    packageStartupMessage(mess2)
  }


}
#' @importFrom tibble tibble
.onLoad <- function(libname, pkgname) {

  run_on_load <- getOption("raadfiles.file.cache.disable")
  file_refresh <- getOption("raadfiles.file.refresh.threshold")
  if (is.null(file_refresh)) {
    file_refresh <- 0.01
    options(raadfiles.file.refresh.threshold = file_refresh)  ## 0 for never, 1 for every time
  }
  if (isTRUE(run_on_load)) {
    assign("message1", "raadfiles in admin-mode, no file list loaded", envir = env0)
    #packageStartupMessage("raadfiles in admin-mode, no file list loaded")
    return(invisible())
  }
  ## this logic says "data roots list is >=1 and I've set the file list/s found to the in-mem cache
  raad_path_was_set <- set_raad_data_roots(use_known_candidates = TRUE,
                                           replace_existing = FALSE, verbose = FALSE)
  if (raad_path_was_set) {
    set_raad_filenames(clobber = FALSE)  ## clobber at start-up, why not
  } else {
    assign("message2", "No existing file cache found, see help('raadfiles-admin') for setting up", envir = env0)
    #packageStartupMessage("No existing file cache found, see help('raadfiles-admin') for setting up")
  }
  ## memoise the one internal search that every collection function goes
  ## through, keyed on its pattern arguments, rather than each exported
  ## function individually (that list drifted: ~30 exported *_files functions
  ## were never memoised). The per-function post-processing (date parsing,
  ## arrange) is cheap on the small result. get_raad_filenames() itself is not
  ## memoised: memoise/cachem calls object.size() on the cached value, which
  ## walks every element of the lazy vroom columns.
  tm <- 24 * 3600
  .find_files_generic <<- memoise::memoize(.find_files_generic, ~memoise::timeout(tm))
  invisible()
}




