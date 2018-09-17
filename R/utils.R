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

get_raadfiles_data_roots <- function() {
  getOption("raadfiles.data.roots")
}
get_raw_raad_filenames <- function() {
  out <- getOption("raadfiles.filename.database" )
  out
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
    cat("global option 'raadfiles.data.roots' set:\n'")
    cat(paste(inputs, collapse = "\n "))
    cat("'\n")

  } else {
    warning("no root paths input")
  }
  invisible(raad_ok)
}
set_raw_raad_filenames <- function() {
  raadfiles.data.roots <- get_raadfiles_data_roots()

  raadfiles.data.filedbs <- file.path(raadfiles.data.roots, ".raad_admin/file_db.rds")
  raadfiles.data.filedbs <- raadfiles.data.filedbs[file.exists(raadfiles.data.filedbs)]
  if (length(raadfiles.data.filedbs) < 1) {
    warning("no file cache found")
    return(invisible(NULL))
  }
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

run_this_function_to_build_raad_cache <- function() {

  roots <- get_raadfiles_data_roots()
  if (length(roots) < 1) {warning("no raad data root directories found")}
  cat(sprintf("Scanning %i root folders for cache listing.\n", length(roots)))
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
    cat(sprintf("%i. found %i files in %s.\n", i, nrow(files), roots[i]))
    saveRDS(files, dbpath, compress = "xz")
  }
  ## trigger update now
  set_raw_raad_filenames()
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
  ##unname(unlist(lapply(possibles, function(pss) grep("/data.*$", fs::dir_ls(pss), value = TRUE))))
  possibles
}


