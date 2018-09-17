## currently we juggling a little some new logic in raadfiles and old logic, ported out of the
## raadfiles branch of raadtools

## default.datadir: there's a test .trysetpath() that does the old way, it ensures many of raadtools functions still work

## raadfiles.default.data.directory - if default.data  is successful, then raadfiles set it's new names, which are more explicit

## cfafiles.default.data.directory - hardcoded for now, no other package does this
## cfafiles.admin.directory  - again hardcoded, so normal user can test the system

## to list the CFA files there is an internal function run_this_function_to_build_cfa_cache to be set as a cron job



run_this_function_to_build_cfa_cache <- function() {
  dp <- getOption("cfafiles.default.data.directory")
  files <- list.files(dp, recursive = TRUE, full.names = TRUE)
  files <- stringr::str_replace(files, sprintf("%s/",  dp), "")
  saveRDS(files, file.path(getOption("cfafiles.admin.directory"), "admin", "filelist", "cfa_allfiles.rds"), compress = FALSE)
}



#' @importFrom tibble tibble
.onLoad <- function(libname, pkgname) {
  op <- options()



  raad_path_was_set <- .trysetpath()


  if (raad_path_was_set) {
    ## RAADTOOLS
    raadfiles.default.data.directory <- unname(getOption("default.datadir"))
    raadfiles.default.data.directory_data <- file.path(raadfiles.default.data.directory, "data")
    ## we have to look into the ./data directory for systems that don't provide visibility
    ## up a level ...
    if (file.exists(raadfiles.default.data.directory_data)) {
      file_RDS_path <- file.path(raadfiles.default.data.directory, "admin", "filelist", "file_db.rds")
      if (!file.exists(file_RDS_path)) {
        warning(paste("cannot file cache:", file_RDS_path))
      } else {
        fs <- readRDS(file_RDS_path)

        op.raadfiles <- list(
          raadfiles.default.data.directory = raadfiles.default.data.directory,
          ## changed here to use the existing cached data frame, not a raw character string MDS 2017-08-22
          raadfiles.filename.database = fs
        )
        toset <- !(names(op.raadfiles) %in% names(op))
        if(any(toset)) options(op.raadfiles[toset])
      }
    }

  }
  ## CFA
  cfafiles.default.data.directory <- "/rdsi/public/CFA"
  cfafiles.admin.directory <- "/mnt/mdsumner_workspace"  ## should be same as data.directory
  if (file.exists(cfafiles.default.data.directory)) {
    files_vector <-try( readRDS(  file.path(cfafiles.admin.directory,  "admin", "filelist", "cfa_allfiles.rds")), silent = TRUE)

    op.cfafiles <- list(
      cfafiles.default.data.directory = cfafiles.default.data.directory,
      cfafiles.admin.directory = cfafiles.admin.directory)
    if (!inherits(files_vector, "try-error")) {
      op.cfafiles[["cfafiles.filename.database"]] <- tibble::tibble(root = cfafiles.default.data.directory,
                                                                    file = files_vector)
    }
    toset <- !(names(op.cfafiles) %in% names(op))
    if(any(toset)) options(op.cfafiles[toset])
  }

  invisible()
}




.possiblepaths <- function() {
  a <- list(default.datadir =  c(
    "/Volumes/files/data",
    "/mnt/aadc/Scientific_Data/Data/gridded_new",
    "//aad.gov.au/files/AADC/Scientific_Data/Data/gridded_new",
    "/mnt/raad",
    "/rdsi/PRIVATE/raad",
    "/rdsi/PUBLIC/raad" ))
  a
}
.trysetpath <- function() {
  possibles <- .possiblepaths()
  success <- FALSE
  existing <- getOption("default.datadir")
  if (!is.null(existing)) {
    possibles <- c(existing, possibles)
  } else {

  }
  for (i in seq_along(possibles)) {
    fi <- file.info(file.path(possibles[i], "data"))
    if (!is.na(fi$isdir) & fi$isdir) {
      options(default.datadir = possibles[i])
      success <- TRUE
      break;
    }
  }


  success
}




