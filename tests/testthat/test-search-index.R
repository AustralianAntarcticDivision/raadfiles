index_file <- function() file.path(local_cache_dir(), "search_index.rds")

test_that("index search is exact against a full-path scan", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  db <- get_raad_filenames(all = TRUE)
  pats <- c("avhrr", "www.ncei.noaa.gov", "cersat", "/netcdf/", "cersat/products", "netcdf/2020", "2020/2020",
            "oisst-avhrr-v02r01.20200105.nc", "avhrr/202001/oisst", "gov/", "/JPL", "example.org/some",
            "other/product/001", "product/001/file_001", "001/file_001.dat", ".dat", "nosuch", "/")
  for (p in pats) {
    i <- raadfiles:::.index_detect(db, p)
    expect_false(is.null(i), info = p)
    expect_identical(i, stringi::stri_detect_fixed(db$file, p), info = p)
  }
  ## regex patterns bypass the index
  expect_null(raadfiles:::.index_detect(db, "nc$"))
  expect_null(raadfiles:::.index_detect(db, "avhrr.*nc"))
  ## bare dots are literal
  expect_true(raadfiles:::.is_literal_pattern("www.ncei.noaa.gov"))
  expect_false(raadfiles:::.is_literal_pattern("[0-9]{8}"))
})

test_that("index is built lazily, persisted, and rebuilt when the listing changes", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  expect_null(get0("search_index", envir = raadfiles:::raadfiles.env))
  expect_false(file.exists(index_file()))
  o <- oisst_daily_files()
  idx <- get0("search_index", envir = raadfiles:::raadfiles.env)
  expect_false(is.null(idx))
  expect_true(file.exists(index_file()))
  expect_length(idx$base, nrow(get_raad_filenames(all = TRUE)))
  expect_identical(idx$key, get("index_key", envir = raadfiles:::raadfiles.env))
  ## a persisted index is picked up by a fresh in-memory state
  assign("search_index", NULL, envir = raadfiles:::raadfiles.env)
  m0 <- file.mtime(index_file())
  expect_identical(raadfiles:::.search_index()$key, idx$key)
  expect_equal(file.mtime(index_file()), m0)
  ## rebuild the listing: key changes, index rebuilt and rewritten
  dir.create(file.path(fx$roots[1], "example.org/new"), recursive = TRUE)
  file.create(file.path(fx$roots[1], "example.org/new/added.nc"))
  Sys.sleep(1.1)
  suppressMessages(capture.output(run_build_raad_cache()))
  expect_null(get0("search_index", envir = raadfiles:::raadfiles.env))
  memoise::forget(raadfiles:::.find_files_generic)
  o2 <- oisst_daily_files()
  idx2 <- get0("search_index", envir = raadfiles:::raadfiles.env)
  expect_false(identical(idx2$key, idx$key))
  expect_length(idx2$base, nrow(get_raad_filenames(all = TRUE)))
  expect_gt(file.mtime(index_file()), m0)
  expect_equal(nrow(o2), nrow(o))
})

test_that("index can be switched off and results agree", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  a <- cersat_daily_files()
  options(raadfiles.search.index = FALSE); on.exit(options(raadfiles.search.index = NULL), add = TRUE)
  memoise::forget(raadfiles:::.find_files_generic)
  expect_null(raadfiles:::.search_index())
  b <- cersat_daily_files()
  expect_identical(a, b)
})

test_that("no local cache: index built in memory, not persisted", {
  fx <- make_fixture(); cleanup <- use_fixture(fx, local_cache = FALSE); on.exit(cleanup())
  o <- oisst_daily_files()
  expect_false(is.null(get0("search_index", envir = raadfiles:::raadfiles.env)))
  expect_false(file.exists(index_file()))
})
