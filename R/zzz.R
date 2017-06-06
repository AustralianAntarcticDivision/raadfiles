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
  saveRDS(files, file.path(getOption("cfafiles.admin.directory"), "admin", "filelist", "cfa_allfiles.rds"), compress = FALSE)
}



#' @importFrom tibble tibble
.onLoad <- function(libname, pkgname) {
  op <- options()



  raad_path_was_set <- .trysetpath()


  if (raad_path_was_set) {
  ## RAADTOOLS
  raadfiles.default.data.directory <- unname(getOption("default.datadir"))
  if (file.exists(raadfiles.default.data.directory)) {
    fs <- NULL
    load(  file.path(raadfiles.default.data.directory, "admin", "filelist", "allfiles2.Rdata"))
    op.raadfiles <- list(
      raadfiles.default.data.directory = raadfiles.default.data.directory,
      raadfiles.filename.database = tibble::tibble(fullname = file.path(raadfiles.default.data.directory, fs))
      )
    toset <- !(names(op.raadfiles) %in% names(op))
    if(any(toset)) options(op.raadfiles[toset])

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
                                                                    file = gsub("^/", "", gsub(cfafiles.default.data.directory, "", files_vector)))
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
    "/rdsi/PRIVATE",
    "/rdsi/PRIVATE/raad"))
  a
}
.trysetpath <- function() {
  possibles <- .possiblepaths()[["default.datadir"]]
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
  ## try RAAD_DIR, which may only be available to R CMD check from ~/.R/check.Renviron
  r <- getOption("repos")
  dd <- getOption("default.datadir")

  if (is.null(dd["default.datadir"])) {
    dd["default.datadir"] <- Sys.getenv("RAAD_DIR");
    options(repos = r, default.datadir = dd);
  }


  success
}




.onAttach <- function(libname, pkgname) {
  raad_path_was_set <- .trysetpath()
  if (!raad_path_was_set) {
    print("Note: raadtools files not available on this system")
    #packageStartupMessage("\nWarning: could not find data repository at any of\n\n",
    #                      paste(.possiblepaths()[["default.datadir"]], collapse = "\n"), sep = "\n\n")

#    packageStartupMessage("Consider setting the option for your system\n")
#    packageStartupMessage('For example: options(default.datadir = "', gsub("\\\\", "/", normalizePath("/myrepository/data", mustWork = FALSE)), '")', '\n', sep = "")

  }
}


