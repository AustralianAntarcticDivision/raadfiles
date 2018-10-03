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

