test_that("literal pattern detection", {
  lit <- raadfiles:::.is_literal_pattern
  expect_true(lit("avhrr"))
  expect_true(lit("NSIDC-0051/2"))
  expect_true(lit("s6250"))
  expect_false(lit("nc$"))
  expect_false(lit("data\\.aad\\.gov\\.au"))
  expect_false(lit("cersat.*daily/.*\\.nc$"))
  expect_false(lit("file[1]"))
  expect_false(lit("a+b"))
})

test_that(".find_files_generic applies patterns in sequence and returns fullname/root", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  f <- raadfiles:::.find_files_generic("www.ncei.noaa.gov")
  expect_named(f, c("fullname", "root"))
  expect_equal(nrow(f), 40)
  expect_true(all(file.exists(f$fullname)))
  ## regex second stage
  f2 <- raadfiles:::.find_files_generic(c("www.ncei.noaa.gov", "2020(01|02)[0-9]{2}\\.nc$"))
  expect_equal(nrow(f2), 40)  ## Jan + first 9 days of Feb = all 40
  f3 <- raadfiles:::.find_files_generic(c("www.ncei.noaa.gov", "202001[0-9]{2}\\.nc$"))
  expect_equal(nrow(f3), 31)
  ## basefile_pattern
  f4 <- raadfiles:::.find_files_generic("www.ncei.noaa.gov", basefile_pattern = "20200105")
  expect_equal(nrow(f4), 1)
  expect_error(raadfiles:::.find_files_generic("does-not-exist"), "no files found")
})

test_that("literal and regex matching agree where both apply", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  db <- get_raad_filenames(all = TRUE)
  a <- raadfiles:::.detect(db$file, "cersat")
  b <- stringi::stri_detect_regex(db$file, "cersat")
  expect_identical(a, b)
  expect_equal(sum(a), 30)
  ## a literal search does not interpret metacharacters
  expect_equal(sum(raadfiles:::.detect(db$file, "file[1]")), 0)
  expect_equal(sum(stringi::stri_detect_fixed(db$file, "file[1]")), 1)
})

test_that("results are memoised on the internal search and survive forget()", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  expect_true(memoise::is.memoised(raadfiles:::.find_files_generic))
  a <- raadfiles:::.find_files_generic("www.ncei.noaa.gov")
  expect_true(memoise::has_cache(raadfiles:::.find_files_generic)("www.ncei.noaa.gov"))
  memoise::forget(raadfiles:::.find_files_generic)
  expect_identical(raadfiles:::.find_files_generic("www.ncei.noaa.gov"), a)
})

test_that("a collection function returns the date/fullname/root contract", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  o <- oisst_daily_files()
  expect_s3_class(o, "tbl_df")
  expect_named(o, c("date", "fullname", "root"))
  expect_equal(nrow(o), 40)
  expect_s3_class(o$date, "POSIXct")
  expect_equal(as.Date(o$date), fx$dates)
  expect_true(!is.unsorted(o$date))
  cs <- cersat_daily_files()
  expect_equal(nrow(cs), 30)
  expect_true(all(c("date", "fullname", "root") %in% names(cs)))
})
