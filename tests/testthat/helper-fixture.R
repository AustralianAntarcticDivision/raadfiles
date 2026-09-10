## Build a small synthetic raad collection: two roots plus a data_deprecated
## root, real files on disk (empty), and file_db.tab caches written by
## run_build_raad_cache(). Everything lives under a tempdir; the local parsed
## copies go to a temp R_USER_CACHE_DIR so nothing touches the user's cache.
make_fixture <- function(n_oisst = 40, n_cersat = 30, n_other = 50) {
  base <- tempfile("raadfixture")
  roots <- file.path(base, c("data", "data_local", "data_deprecated"))
  for (r in roots) dir.create(r, recursive = TRUE, showWarnings = FALSE)

  ymd <- function(n) {
    d <- seq(as.Date("2020-01-01"), by = "1 day", length.out = n)
    list(date = d, ymd = format(d, "%Y%m%d"), ym = format(d, "%Y%m"))
  }
  o <- ymd(n_oisst)
  oisst <- sprintf("www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/%s/oisst-avhrr-v02r01.%s.nc",
                   o$ym, o$ymd)
  c1 <- ymd(n_cersat)
  cersat <- sprintf("ftp.ifremer.fr/ifremer/cersat/products/gridded/psi-concentration/data/antarctic/daily/netcdf/%s/%s.nc",
                    substr(c1$ymd, 1, 4), c1$ymd)
  other <- sprintf("example.org/some/other/product/%03d/file_%03d.dat", seq_len(n_other), seq_len(n_other))
  ## a file whose name contains regex metacharacters, to check literal matching
  odd <- "example.org/odd/a+b(c)/file[1].txt"

  files <- list(c(oisst, other[1:25]), c(cersat, other[26:n_other], odd), c("example.org/deprecated/old.nc"))
  for (i in seq_along(roots)) {
    for (f in files[[i]]) {
      p <- file.path(roots[i], f)
      dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
      file.create(p)
    }
  }
  list(base = base, roots = roots, n = lengths(files), dates = o$date)
}

## point raadfiles at a fixture: fresh cache dir, roots set, listing built
use_fixture <- function(fx, local_cache = TRUE) {
  cache_dir <- file.path(fx$base, "usercache")
  dir.create(cache_dir, showWarnings = FALSE)
  old_env <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_dir)
  old_opts <- options(raadfiles.data.roots = fx$roots,
                      raadfiles.local.cache = local_cache,
                      raadfiles.file.refresh.threshold = 0,
                      raadfiles.database.status = NULL)
  memoise::forget(raadfiles:::.find_files_generic)
  suppressMessages(capture.output(raadfiles::run_build_raad_cache()))
  function() {
    options(old_opts)
    if (is.na(old_env)) Sys.unsetenv("R_USER_CACHE_DIR") else Sys.setenv(R_USER_CACHE_DIR = old_env)
    memoise::forget(raadfiles:::.find_files_generic)
    unlink(fx$base, recursive = TRUE)
  }
}

local_cache_dir <- function() tools::R_user_dir("raadfiles", "cache")
