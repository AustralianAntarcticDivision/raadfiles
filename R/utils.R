globalVariables("desc")  ## for dplyr in arrange()



.find_files_generic <- function(pattern, basefile_pattern = "", ...) {
  ## maybe tolower both?

  files <- get_raad_filenames(all = TRUE)
  ## allow input of multiple patterns for slow searchers
  for (pattern0 in pattern) {
    files <- dplyr::filter(files, stringr::str_detect(.data$file, pattern0))
    if (nrow(files) < 1)
      stop("no files found")
  }

  if (nzchar(basefile_pattern[1L])) {
    for (pattern1 in basefile_pattern) {
      files <- dplyr::filter(files, stringr::str_detect(.data$file, pattern1))
      if (nrow(files) < 1) break;
    }
  }

  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), root = .data$root)

  files
}



set_utc_md <- function(x) {
  attr(x, "tzone") <- "UTC"
  x
}
## apply UTC metadata for print (raadtools#72)
set_dt_utc <- function(x) {
  x[["date"]] <- set_utc_md(x[["date"]])

  ## and do sanity checks on last 20 files
  files <- x[["fullname"]]
  if (is.null(files)) {
    files <- x[["ufullname"]]
    if (is.null(files)) files <- x[["vfullname"]]
  }

  if (!is.null(files)) {
    sizes <- fs::file_size(tail(files, 20L))
    bad <- !sizes > 0
   if (any(bad)) {
     message(sprintf("\n(please inform maintainers)\n\n problem with zero-size files near\n %s\n", files[which(bad)[1L]]))
      idx <- c(rep(TRUE, dim(x)[1L] - 20), !bad)
      x <- x[idx, ]
   }
  }
  x
}


## file cache utilities

remove_leading_slash <- function(x) {
  gsub("^/+", "", gsub("^\\\\+", "", x))
}

