globalVariables("desc")  ## for dplyr in arrange()

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

