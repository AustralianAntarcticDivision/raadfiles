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


## file cache utilities

remove_leading_slash <- function(x) {
  gsub("^/+", "", gsub("^\\\\+", "", x))
}

