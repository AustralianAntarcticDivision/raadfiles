globalVariables("desc")  ## for dplyr in arrange()



## does a pattern contain any regex metacharacters? if not we can match it
## with a fixed (literal) search, which is several times faster than a regex
## over millions of file names
.is_literal_pattern <- function(x) {
  !grepl("[][\\^$.|?*+(){}]", x)
}

## detect pattern in x, choosing fixed or regex matching
.detect <- function(x, pattern) {
  if (.is_literal_pattern(pattern)) {
    stringi::stri_detect_fixed(x, pattern)
  } else {
    stringi::stri_detect_regex(x, pattern)
  }
}

.find_files_generic <- function(pattern, basefile_pattern = "", ...) {
  ## maybe tolower both?

  files <- get_raad_filenames(all = TRUE)
  ## allow input of multiple patterns for slow searchers, each successive
  ## pattern is applied to the (usually much smaller) surviving set, so put
  ## the cheapest/most selective literal first
  for (pattern0 in pattern) {
    idx <- which(.detect(files[["file"]], pattern0))
    if (length(idx) < 1)
      stop("no files found")
    files <- files[idx, ]
  }

  if (nzchar(basefile_pattern[1L])) {
    for (pattern1 in basefile_pattern) {
      idx <- which(.detect(files[["file"]], pattern1))
      if (length(idx) < 1) break;
      files <- files[idx, ]
    }
  }

  tibble::tibble(fullname = file.path(files[["root"]], files[["file"]]), root = files[["root"]])
}


## file cache utilities

remove_leading_slash <- function(x) {
  gsub("^/+", "", gsub("^\\\\+", "", x))
}

