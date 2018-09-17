## currently we juggling a little some new logic in raadfiles and old logic, ported out of the
## raadfiles branch of raadtools

## default.datadir: there's a test .trysetpath() that does the old way, it ensures many of raadtools functions still work

## raadfiles.default.data.directory - if default.data  is successful, then raadfiles set it's new names, which are more explicit

## cfafiles.default.data.directory - hardcoded for now, no other package does this
## cfafiles.admin.directory  - again hardcoded, so normal user can test the system

## to list the CFA files there is an internal function run_this_function_to_build_cfa_cache to be set as a cron job


.possiblepaths <- function() {
  c(
    "/Volumes/files/data",
    "/mnt/aadc/Scientific_Data/Data/gridded_new",
    "//aad.gov.au/files/AADC/Scientific_Data/Data/gridded_new",
    "/mnt/raad",
    "/rdsi/PRIVATE/raad",
    "/rdsi/PUBLIC/raad" )
}

get_raadfiles_data_roots <- function() {
  getOption("raadfiles.data.roots")
}
remove_leading_slash <- function(x) {
  gsub("^/+", "", gsub("^\\\\+", "", x))
}
run_this_function_to_build_raad_cache <- function() {

  roots <- get_raadfiles_data_roots()
  if (length(roots) < 1) {warning("no raad data root directories found")}
  cat(sprintf("Scanning %i root folders for cache listing", length(roots)))
  for (i in seq_along(roots)) {
    adminpath <- file.path(roots[i], ".raad_admin")
    dir.create(adminpath, showWarnings = FALSE)
    dbpath <- file.path(adminpath, "file_db.rds")
    filenames <- as.character(fs::dir_ls(roots[i], all = TRUE, recursive = TRUE))
    if (is.null(filenames)) {
      files <- tibble::tibble(root = character(0), file = character(0))

    } else {
      ## fix up root-less file
      filenames <- remove_leading_slash(gsub(roots[i], "", filenames))

      files <- tibble::tibble(root = roots[i], file = filenames)

    }


    print(sprintf("%i. found %i files in %s", i, nrow(files), roots[i]))
    saveRDS(files, dbpath, compress = FALSE)
  }
  ## trigger update now
  set_raw_raad_filenames()
}


get_raw_raad_filenames <- function() {
  out <- getOption("raadfiles.filename.database" )
  out
}
# get_raad_datadir <- function() {
#   getOption("raadfiles.default.data.directory")
# }


set_raw_raad_filenames <- function() {
  raadfiles.data.roots <- get_raadfiles_data_roots()

  raadfiles.data.filedbs <- file.path(raadfiles.data.roots, ".raad_admin/file_db.rds")
  fslist <- lapply(raadfiles.data.filedbs, readRDS)
  for (i in seq_along(fslist)) {
    x <- fslist[[i]]
    x$root <- raadfiles.data.roots[i]
    fslist[[i]] <- x
  }
  fs <- dplyr::bind_rows(fslist)
  options(raadfiles.filename.database = fs)
  invisible(NULL)
}

#' @importFrom tibble tibble
.onLoad <- function(libname, pkgname) {
  raad_path_was_set <- set_raadfile_data_roots(use_known_candidates = TRUE, replace_existing = FALSE)
  if (raad_path_was_set) {
    set_raw_raad_filenames()
  }
  invisible()
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
  unname(unlist(lapply(possibles, function(pss) grep("/data.*$", fs::dir_ls(pss), value = TRUE))))
}
set_raadfile_data_roots <- function(..., use_known_candidates = FALSE, replace_existing = TRUE) {
  inputs <- validate_input_paths(...)
  if (use_known_candidates) {
    inputs <- c(inputs, validate_possible_paths())
  }
  if (!replace_existing) {

    ## get existing, there may be a pre-load hook for this
    existing <- get_raadfiles_data_roots()
    inputs <- c(inputs, existing)
  }
  inputs <- unique(inputs)

  raad_ok <- FALSE
  if (length(inputs) > 0) raad_ok <- TRUE
  if (raad_ok) {
    options(raadfiles.data.roots = inputs)
    cat("global option 'raadfiles.data.roots'' set:\n'")
    cat(paste(inputs, collapse = ", "))
    cat("\n")

  } else {
    stop("no root paths input")
  }
  invisible(raad_ok)
}




