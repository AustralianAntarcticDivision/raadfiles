## Every exported *_files() function that finds something on the fixture must
## return a tibble whose leading columns are date (if any), fullname, root.
## Functions that find nothing on the fixture error with "no files found" and
## are reported as skipped so the count is visible; anything else is a failure.
test_that("collection functions honour the date/fullname/root contract", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  ex <- getNamespaceExports("raadfiles")
  fns <- sort(grep("files", ex, value = TRUE))
  ## known exceptions: paired u/v files; remote-source generator (see NEWS)
  exceptions <- c("altimetry_daily_files", "ghrsst_daily_files")
  ran <- character(); none <- character(); defunct <- character()
  for (f in fns) {
    fun <- get(f, asNamespace("raadfiles"))
    res <- tryCatch(suppressWarnings(fun()), error = function(e) e)
    if (inherits(res, "defunctError") || (inherits(res, "error") && grepl("defunct", conditionMessage(res)))) {
      defunct <- c(defunct, f); next
    }
    if (inherits(res, "error")) {
      expect_match(conditionMessage(res), "no files found|cannot find", info = f)
      none <- c(none, f); next
    }
    ran <- c(ran, f)
    if (f %in% exceptions) next
    expect_s3_class(res, "tbl_df")
    lead <- intersect(c("date", "fullname", "root"), names(res))
    expect_true(all(c("fullname", "root") %in% names(res)), info = f)
    expect_identical(names(res)[seq_along(lead)], lead, info = f)
    expect_true(all(file.exists(res$fullname)), info = f)
    if ("date" %in% names(res)) expect_s3_class(res$date, "POSIXct")
  }
  ## the fixture exercises at least these
  expect_true(all(c("oisst_daily_files", "cersat_daily_files", "nsidc_daily_files_v2",
                    "sose_monthly_files", "rema_100m_files", "rema_100m_slope_files",
                    "rema_8m_slope_files", "rema_tile_files", "rema_200m_files") %in% ran))
  message(sprintf("contract: %d ran, %d found nothing on the fixture, %d defunct",
                  length(ran), length(none), length(defunct)))
})

test_that(".raad_files_result reorders and keeps extras", {
  x <- data.frame(root = "r", extra = 1, fullname = "f", date = Sys.time())
  y <- raadfiles:::.raad_files_result(x)
  expect_s3_class(y, "tbl_df")
  expect_named(y, c("date", "fullname", "root", "extra"))
  z <- raadfiles:::.raad_files_result(tibble::tibble(fullname = "f", root = "r"))
  expect_named(z, c("fullname", "root"))
})

test_that("defunct rema tiles function signals defunct", {
  expect_error(rema_8m_tiles(), "defunct")
})
