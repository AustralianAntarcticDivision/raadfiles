globalVariables("desc")  ## for dplyr in arrange()



## Does a pattern contain regex metacharacters? If not it is matched with a
## fixed (literal) search, several times faster than a regex over millions of
## file names and eligible for the search index. An unescaped '.' on its own
## does not make a pattern a regex here: the collection functions write host
## names and file names with bare dots ("www.ncei.noaa.gov", "sst.mon.mean.nc")
## and mean them literally; a '.' used as a wildcard alongside other regex
## syntax ([0-9], .*, $) still goes to the regex path.
.is_literal_pattern <- function(x) {
  !grepl("[][\\^$|?*+(){}]", x)
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
  ## Work on the column vectors, never on the tibble: '[' on a vroom column
  ## is a lazy subset, but '[.tbl_df' (vctrs) materialises the whole column.
  file <- files[["file"]]
  root <- files[["root"]]
  ## allow input of multiple patterns for slow searchers, each successive
  ## pattern is applied to the (usually much smaller) surviving set, so put
  ## the cheapest/most selective literal first. The first literal pattern goes
  ## through the directory/basename index when there is one (search-index.R),
  ## which avoids touching the full path strings at all.
  first <- TRUE
  for (pattern0 in pattern) {
    hit <- if (first) .index_detect(files, pattern0) else NULL
    if (is.null(hit)) hit <- .detect(file, pattern0)
    first <- FALSE
    idx <- which(hit)
    if (length(idx) < 1)
      stop("no files found")
    file <- file[idx]
    root <- root[idx]
  }

  if (nzchar(basefile_pattern[1L])) {
    for (pattern1 in basefile_pattern) {
      idx <- which(.detect(file, pattern1))
      if (length(idx) < 1) break;
      file <- file[idx]
      root <- root[idx]
    }
  }

  tibble::tibble(fullname = file.path(root, file), root = root)
}


## Column contract for collection functions: date (when present), fullname,
## root, then anything else in the order given. Every *_files() function
## should return through this.
.raad_files_result <- function(x) {
  lead <- intersect(c("date", "fullname", "root"), names(x))
  x <- x[c(lead, setdiff(names(x), lead))]
  tibble::as_tibble(x)
}

## file cache utilities

remove_leading_slash <- function(x) {
  gsub("^/+", "", gsub("^\\\\+", "", x))
}

