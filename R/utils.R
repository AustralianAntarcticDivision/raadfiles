globalVariables("desc")  ## for dplyr in arrange()

set_utc_md <- function(x) {
  attr(x, "tzone") <- "UTC"
  x
}
## apply UTC metadata for print (raadtools#72)
set_dt_utc <- function(x) {
  x[["date"]] <- set_utc_md(x[["date"]])
  x
}


## file cache utilities

remove_leading_slash <- function(x) {
  gsub("^/+", "", gsub("^\\\\+", "", x))
}


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

#' Raadfile administration tools
#'
#' Administration tools for managing a data library.
#'
#' `get_raad_data_roots` returns the current list of visible root directories
#'
#' `get_raad_filenames` returns the entire list of all files found in visible root directories
#'
#' `set_raad_filenames` runs the system to update the file listing and refresh it
#'
#' `set_raad_data_roots` set data root paths, default use is to apply the strings input as the exclusive set
#'
#' `raad_filedb_path` return the path to the database of file names for each input directory
#' 
#' @export
#' @rdname raad-admin
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
#' @export
#' @rdname raad-admin
get_raad_filenames <- function() {
  out <- getOption("raadfiles.filename.database" )
  if (is.null(out) || nrow(out) < 1) {
    roots <-  get_raadfiles_data_roots()
    mess <- "no files found in the 'raadfiles.filename.database'"
    if (is.null(roots)) {
      mess <- paste0(mess, "\nand no root directories found, try setting 'set_raadfiles_data_roots()'.")

    }
    mess <- paste0(mess, "\nSee installation instructions.")
    stop(mess)
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
#' @rdname raad-admin
set_raad_data_roots <- function(..., replace_existing = TRUE, use_known_candidates = FALSE) {
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

  raad_ok <- FALSE
  if (length(inputs) > 0) raad_ok <- TRUE
  if (raad_ok) {
    options(raadfiles.data.roots = inputs)
    cat("global option 'raadfiles.data.roots' set:\n'")
    cat(paste(inputs, collapse = "\n "))
    cat("'\n")

  } else {
    warning("no root paths input")
  }
  invisible(raad_ok)
}


## function that returns the path to the file_db file
## this is used by raadtools::set_data_roots, so is exported
#' @export
#' @rdname raad-admin
raad_filedb_path <- function(...) {
    file.path(unlist(list(...)), ".raad_admin/file_db.rds")
}

set_raw_raad_filenames <- function() {
  .Deprecated("set_raad_filenames")
  set_raad_filenames()
}
#' @param clobber by default do not ignore existing file cache, set to TRUE to ignore and set
#' @export
#' @rdname raad-admin
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
      raadf <- get_raad_filenames()
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
#' @export
#' @aliases get_raad_time_stamp
set_raad_time_stamp <- function(x) {
  attr(x, "raad_time_stamp") <- Sys.time()
  x
}
#' @name set_raad_time_stamp
#' @export
get_raad_time_stamp <- function() {
  attr(get_raad_filenames(), "raad_time_stamp")
}
run_this_function_to_build_raad_cache <- function() {

  roots <- get_raad_data_roots()
  if (length(roots) < 1) {warning("no raad data root directories found")}
  tok1 <- c("directory", "directories")[(length(roots) > 1)+1]
  cat(sprintf("Scanning %i root %s for cache listing.\n", length(roots), tok1))
  for (i in seq_along(roots)) {
    adminpath <- dirname(raad_filedb_path(roots[i]))
    dir.create(adminpath, showWarnings = FALSE)
    dbpath <- raad_filedb_path(roots[i])
    filenames <- as.character(fs::dir_ls(roots[i], all = TRUE, recursive = TRUE,
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


