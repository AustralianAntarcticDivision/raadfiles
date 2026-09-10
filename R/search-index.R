## Search index for the file listing
##
## Every collection function is a substring search over the 'file' column of
## the listing (millions of relative paths). Scanning those full paths costs
## around a second per search, and the first scan in a session also pays to
## materialise the lazily read strings. Splitting each path once into its
## directory and basename gives an exact, much cheaper search for literal
## patterns:
##
##   path contains P  <=>  dir contains P
##                         or basename contains P            (P has no '/')
##                         or dir ends with P_head and
##                            basename starts with P_tail    (P = P_head/P_tail)
##
## since a basename holds no '/', an occurrence of P crossing the dir/basename
## boundary must have P's last '/' on that boundary. There are only thousands
## of unique directories, so the dir tests are free; the basename tests run
## over short strings.
##
## The index is built lazily on the first search after the listing changes,
## from the local copies of the listing when there are any (an eager vroom
## read materialises far faster than element-wise ALTREP access), and is
## persisted as search_index.rds beside them, keyed by the listing signature.
## Option raadfiles.search.index = FALSE turns all of this off.

.search_index <- function() {
  if (!isTRUE(getOption("raadfiles.search.index", TRUE))) return(NULL)
  env <- raadfiles.env
  key <- get0("index_key", envir = env, inherits = FALSE)
  if (is.null(key)) return(NULL)
  idx <- get0("search_index", envir = env, inherits = FALSE)
  if (!is.null(idx) && identical(idx$key, key)) return(idx)

  cache_dir <- if (isTRUE(getOption("raadfiles.local.cache", TRUE))) local_cache_dir() else NULL
  idx_file <- if (!is.null(cache_dir)) file.path(cache_dir, "search_index.rds") else NULL

  ## persisted copy from an earlier session?
  if (!is.null(idx_file) && file.exists(idx_file)) {
    idx <- tryCatch(readRDS(idx_file), error = function(e) NULL)
    if (!is.null(idx) && identical(idx$key, key) && .index_matches(idx, env)) {
      assign("search_index", idx, envir = env)
      return(idx)
    }
  }

  idx <- .build_search_index(env, key)
  if (is.null(idx)) return(NULL)
  assign("search_index", idx, envir = env)
  if (!is.null(idx_file)) {
    tryCatch({
      tmp <- tempfile("search_index", tmpdir = cache_dir, fileext = ".rds")
      saveRDS(idx, tmp, compress = FALSE)
      if (!file.rename(tmp, idx_file)) unlink(tmp)
    }, error = function(e) NULL)
  }
  idx
}

## the index must line up row-for-row with the listing in memory
.index_matches <- function(idx, env) {
  fs <- get0("raadfiles.filename.database", envir = env, inherits = FALSE)
  !is.null(fs) && length(idx$base) == nrow(fs)
}

.build_search_index <- function(env, key) {
  fs <- get0("raadfiles.filename.database", envir = env, inherits = FALSE)
  if (is.null(fs) || nrow(fs) < 1) return(NULL)
  read_dbs <- get0("read_dbs", envir = env, inherits = FALSE)
  ## prefer an eager re-read of the listing over materialising the lazy column
  f <- NULL
  if (!is.null(read_dbs) && all(file.exists(read_dbs))) {
    f <- tryCatch(vroom::vroom(read_dbs, col_types = vroom::cols_only(file = vroom::col_character()),
                               progress = FALSE, altrep = FALSE)[["file"]],
                  error = function(e) NULL)
    if (!is.null(f) && length(f) != nrow(fs)) f <- NULL
  }
  if (is.null(f)) f <- fs[["file"]]
  base <- basename(f)
  dirs <- dirname(f)
  dirs[dirs == "."] <- ""
  udirs <- unique(dirs)
  list(key = key, dirs = udirs, dir_index = match(dirs, udirs), base = base)
}

## detect a literal pattern through the index; NULL means "no index, scan"
.index_detect <- function(files, pattern) {
  if (!.is_literal_pattern(pattern)) return(NULL)
  idx <- .search_index()
  if (is.null(idx) || length(idx$base) != nrow(files)) return(NULL)
  in_dir <- stringi::stri_detect_fixed(idx$dirs, pattern)[idx$dir_index]
  slash <- regexpr("/[^/]*$", pattern)
  if (slash < 0L) {
    in_dir | stringi::stri_detect_fixed(idx$base, pattern)
  } else {
    head <- substr(pattern, 1L, slash - 1L)
    tail <- substr(pattern, slash + 1L, nchar(pattern))
    ends <- if (nzchar(head)) stringi::stri_endswith_fixed(idx$dirs, head)[idx$dir_index] else TRUE
    starts <- if (nzchar(tail)) stringi::stri_startswith_fixed(idx$base, tail) else TRUE
    in_dir | (ends & starts)
  }
}
