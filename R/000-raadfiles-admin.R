
.possiblepaths <- function() {
  c(
    "/Volumes/files/data",
    "/mnt/AADC/Scientific_Data/Data/gridded_new/data",
    "/mnt/AADC/Scientific_Data/Data/gridded_new/data_local",
    "/mnt/AADC/Scientific_Data/Data/gridded_new/data_staging",
    "/mnt/AADC/Scientific_Data/Data/gridded_new/data_deprecated",
    "//aad.gov.au/files/AADC/Scientific_Data/Data/gridded_new/data",
    "//aad.gov.au/files/AADC/Scientific_Data/Data/gridded_new/data_local",
    "//aad.gov.au/files/AADC/Scientific_Data/Data/gridded_new/data_staging",
    "//aad.gov.au/files/AADC/Scientific_Data/Data/gridded_new/data_deprecated",
    "/mnt/raad",
    "/rdsi/PRIVATE/raad/data",    "/rdsi/PRIVATE/raad/data_local",     "/rdsi/PRIVATE/raad/data_staging", "/rdsi/PRIVATE/raad/data_deprecated",
    "/rdsi/PUBLIC/raad/data" )
}

#' Raadfiles administration tools
#'
#' Administration tools for managing a data library.
#'
#' These management functions are aimed at raadtools users, but can be
#' used for any file collection. The administration tools consist of **data roots** and
#' control over the building, reading, and caching of the available file list. No
#' interpretation of the underlying files is provided in the administration tools.
#'
#' A typical user won't use these functions but may want to investigate the contents of the raw file list, with
#' `get_raad_filenames()`.
#'
#' A user setting up a raadfiles collection will typically set the root directory/directories with `set_raad_data_roots()`,
#' then run the file cache list builder with `run_build_raad_cache()`, and then `set_raad_filenames()`
#' to actually load the file cache into memory.
#'
#' In a new R session there is no need to run `set_raad_filenames()` directly as this
#' will be done as the package loads. To disable this automatic behaviour use `options(raadfiles.file.cache.disable = TRUE)` *before* the package is
#' used or loaded. This is typically done when calling `run_build_raad_cache()` in a cron task.
#'
#' Every raadfiles file collection function (e.g. `oisst_daily_files`) will run `get_raad_filenames` to obtain the full raw list of
#' available files from the global in-memory option `getOption("raadfiles.filename.database")` and there is a low threshold probability that
#' this will also trigger a re-read of the file listing from the root directories. To avoid this trigger either use `getOption("raadfiles.filename.database")`
#' directly to get the in-memory file list, or set `options(raadfiles.file.refresh.threshold = 0)` to prevent the trigger. (Set it to 1 to force it always
#' to be read, also controlled by `set_raad_filenames(clobber = TRUE)`).
#'
#'
#' There is a family of functions and global options used for administration.
#'
#' @section Administration functions:
#'
#' \tabular{ll}{
#'  \code{\link{set_raad_data_roots}}  \tab set data root paths, for normal use only one data root is needed \cr
#'  \code{\link{set_raad_filenames}}   \tab runs the system to update the file listing and refresh it  \cr
#'  \code{\link{get_raad_data_roots}}  \tab returns the current list of visible root directories \cr
#'  \code{\link{get_raad_filenames}}   \tab returns the entire list of all files found in visible root directories \cr
#'  \code{\link{run_build_raad_cache}} \tab scan all root directories and update the file listing in each  \cr
#'  }
#'
#'
#'
#' @section Options for use by administrators:
#'
#' \tabular{ll}{
#'  \code{raadfiles.data.roots} \tab the list of paths to root directories \cr
#'  \code{raadfiles.file.cache.disable} \tab disable on-load setting of the in-memory file cache (never set automatically by the package)  \cr
#'  \code{raadfiles.file.refresh.threshold} \tab threshold probability of how often to refresh in-memory file cache (0 = never, 1 = every time `get_raad_filenames()` is called) \cr
#' }
#'
#'
#' @section Internal options, used by the package:
#' Options used internally, and subject to control by adminstrator options and the running of admin functions (they may not be set).
#'
#' \tabular{ll}{
#'  \code{raadfiles.filename.database} \tab the data frame of all file names from the data roots \cr
#'  \code{raadfiles.database.status} \tab a status record of the in-memory filename database (timestamp) \cr
#'  }
#'
#' @export
#' @name raadfiles-admin
#' @aliases raadfiles-admin get_raad_filenames set_raad_data_roots   raad_filedb_path set_raad_filenames run_build_raad_cache
#'
get_raad_data_roots <- function() {
  getOption("raadfiles.data.roots")
}
get_raw_raad_filenames <- function() {
  .Deprecated("get_raad_filenames")
  get_raad_filenames()
}
get_raadfiles_data_roots <- function() {
  .Deprecated("get_raad_data_roots")
  get_raad_data_roots()
}
#' @param all if `TRUE` include 'data_deprecated', expert-use only
#'
#' @export
#' @rdname raadfiles-admin
get_raad_filenames <- function(all = FALSE) {
  out <- getOption("raadfiles.filename.database" )
  file_refresh <- getOption("raadfiles.file.refresh.threshold")
  if (is.null(out) || nrow(out) < 1) {
    roots <-  get_raad_data_roots()
    mess <- "no files found in the 'raadfiles.filename.database'"
    if (is.null(roots)) {
      mess <- paste0(mess, "\nand no root directories found.")
      if (isTRUE(getOption("raadfiles.file.cache.disable"))) {
        mess <- paste0(mess, "\n\noption(raadfiles.file.cache.disable) is TRUE, maybe you want to unset that?")
      }
      message(mess)
      return(tibble::tibble(root = character(0), file = character(0)))
    }

  }
  if (file_refresh > 0 && runif(1, 0, 1) > (1 - file_refresh)) {
    set_raad_filenames()
  }
  if (!all) {
    ## trim out specific files
    out <- dplyr::filter(out, !stringr::str_detect(root, "/data_deprecated"))
    out <- dplyr::filter(out, !stringr::str_detect(root, "PRIVATE/raad/data"))

  }

  out
}
set_raadfile_data_roots <- function(..., replace_existing = TRUE, use_known_candidates = FALSE) {
  .Deprecated("set_raad_data_roots")
  set_raad_data_roots(..., use_known_candidates = use_known_candidates, replace_existing = replace_existing)
}
#' @param ... input file paths to set
#' @param replace_existing replace existing paths, defaults to TRUE
#' @param use_known_candidates apply internal logic for known candidates (for internal use at raad-hq), defaults to FALSE
#'
#' @export
#' @rdname raadfiles-admin
set_raad_data_roots <- function(..., replace_existing = TRUE, use_known_candidates = FALSE,
                                verbose = TRUE) {
  inputs <- validate_input_paths(...)
  if (use_known_candidates) {
    inputs <- c(inputs, validate_possible_paths())
  }
  if (!replace_existing) {

    ## get existing, there may be a pre-load hook for this
    existing <- get_raad_data_roots()
    inputs <- c(inputs, existing)
  }
  inputs <- unique(inputs)
  ## find out last modified time of each DB cache
  mtimes <- format(file.info(file.path(inputs, ".raad_admin", "file_db.rds"))[,"mtime"])

  if (any(is.na(mtimes))) mtimes[is.na(mtimes)] <- ""
  maxchar <- max(nchar(inputs) + nchar(mtimes))
  padding_n <- maxchar - nchar(inputs) - nchar(mtimes)
  padding <- unlist(lapply(padding_n, function(x) paste(rep(" ", x + 4), collapse = "")))
  raad_ok <- FALSE
  if (length(inputs) > 0) raad_ok <- TRUE
  if (raad_ok) {
    options(raadfiles.data.roots = inputs)
    cat("global option 'raadfiles.data.roots' set:\n'")
    deets <- paste(inputs, padding, mtimes, sep = "")
    cat(paste(deets, collapse = "\n "))
    cat("'\n")

  } else {
    if (verbose) warning("no root paths input")
  }
  invisible(raad_ok)
}


## function that returns the path to the file_db file
## this is used by raadtools::set_data_roots, so is exported
#' @export
#' @rdname raadfiles-admin
raad_filedb_path <- function(...) {
  file.path(unlist(list(...)), ".raad_admin/file_db.rds")
}

set_raw_raad_filenames <- function() {
  .Deprecated("set_raad_filenames")
  set_raad_filenames()
}
#' @param clobber by default do not ignore existing file cache, set to TRUE to ignore and set
#' @export
#' @rdname raadfiles-admin
set_raad_filenames <- function(clobber = FALSE) {
  raadfiles.data.roots <- get_raad_data_roots()

  raadfiles.data.filedbs <- raad_filedb_path(raadfiles.data.roots)
  raadfiles.data.filedbs <- raadfiles.data.filedbs[file.exists(raadfiles.data.filedbs)]
  if (length(raadfiles.data.filedbs) < 1) {
    warning("no file cache found")
    return(invisible(NULL))
  }

  ## record the db hashes
  data_dbs <- tibble::tibble(db = raadfiles.data.filedbs,
                             md5 = unlist(lapply(raadfiles.data.filedbs, digest::digest, algo = "md5", file = TRUE)),
                             file_ok = TRUE)

  if (!clobber) {

    current_dbs <- getOption("raadfiles.database.status")
    if (!is.null(current_dbs)) {
      if (nrow(dplyr::distinct(dplyr::inner_join(data_dbs, current_dbs, c("db", "md5")))) == nrow(data_dbs)) {
        ## no need to update
        ## don't run get_raad_filenames logic here, because that calls this function with threshold prob
        raadf <- getOption("raadfiles.filename.database" )
        message(sprintf("Raad file cache is up to date as at %s (%i files listed) \n", format(attr(raadf, "raad_time_stamp")), nrow(raadf)))
        return(invisible(NULL))
      }
    }
  }
  fslist <- vector("list", length(raadfiles.data.filedbs))
  for (i in seq_along(fslist)) {
    db <- try(readRDS(raadfiles.data.filedbs[i]), silent = TRUE)
    if (!inherits(db, "try-error")) {
      fslist[[i]] <- db
    } else {
      warning(sprintf("failure to read '%s': is file corrupt?\n Consider re-running file cache creation. ", raadfiles.data.filedbs[i]))
      data_dbs$file_ok[i] <- FALSE
    }
  }
  ##fslist <- lapply(raadfiles.data.filedbs, fst::read.fst)
  for (i in seq_along(fslist)) {
    x <- fslist[[i]]
    if (is.null(x)) next;
    #x[["root"]] <- rep(raadfiles.data.roots[i], nrow(x))
    fslist[[i]][["root"]] <- rep(raadfiles.data.roots[i], nrow(x))
  }
  fs <- dplyr::bind_rows(fslist)
  ## time stamp it
  fs <- set_raad_time_stamp(fs)
  message(sprintf("Uploading raad file cache as at %s (%i files listed) \n", format(attr(fs, "raad_time_stamp")), nrow(fs)))

  options(raadfiles.filename.database = fs, raadfiles.database.status = data_dbs)
  invisible(NULL)
}

#' Set a time stamp on a data frame
#'
#' Used by `set_raad_filenames` when uploading the cache to memory.
#' @param x data frame
#'
#' @return x with attribute "raad_time_stamp" set
#' @noRd
#' @aliases get_raad_time_stamp
set_raad_time_stamp <- function(x) {
  attr(x, "raad_time_stamp") <- Sys.time()
  x
}
#' @name set_raad_time_stamp
#' @noRd
get_raad_time_stamp <- function() {
  attr(get_raad_filenames(), "raad_time_stamp")
}

run_this_function_to_build_raad_cache <- function() {
  .Deprecated("run_build_raad_cache")
  run_build_raad_cache()
}
#' @name raadfiles-admin
#' @export
run_build_raad_cache <- function() {

  roots <- get_raad_data_roots()
  if (length(roots) < 1) {warning("no raad data root directories found")}
  tok1 <- c("directory", "directories")[(length(roots) > 1)+1]
  cat(sprintf("Scanning %i root %s for cache listing.\n", length(roots), tok1))
  for (i in seq_along(roots)) {
    adminpath <- dirname(raad_filedb_path(roots[i]))
    dir.create(adminpath, showWarnings = FALSE)
    dbpath <- raad_filedb_path(roots[i])
    filenames <- as.character(fs::dir_ls(roots[i], all = TRUE, recurse = TRUE,
                                         ## no directory, FIFO, socket, character_device or block_device
                                         type = c("file", "symlink")))
    if (is.null(filenames)) {
      files <- tibble::tibble(root = character(0), file = character(0))

    } else {
      ## fix up root-less file
      filenames <- remove_leading_slash(gsub(roots[i], "", filenames))

      files <- tibble::tibble(root = roots[i], file = filenames)

    }
    tok <- c("file", "files")[(nrow(files) > 1)+1]
    cat(sprintf("%i). Found %i %s in %s.\n", i, nrow(files), tok, roots[i]))
    saveRDS(files, dbpath, compress = "xz")
    #fst::write.fst(files, dbpath)
  }
  ## trigger update now
  set_raad_filenames(clobber = TRUE)
}

validate_input_paths <- function(...) {
  inputs <- unname(unlist(list(...)))
  if (length(inputs) < 1) {
    ## warning("no inputs provided")
    return(NULL)
  }
  out <- inputs[file.exists(inputs)]
  if (length(out) < 1) {
    warning(sprintf("root directory/s %s not found", paste(out, collapse = ",")))
    return(NULL)
  }
  out
}

validate_possible_paths <- function() {
  possibles <- .possiblepaths()
  possibles <- possibles[file.exists(possibles)]
  possibles
}


