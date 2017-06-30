context("ice-files")

s <- nsidc_south_monthly_files()
n <- nsidc_north_monthly_files()
a <- nsidc_monthly_files()
test_that("file stamps are sensible", {
  dts <- unclass(range(diff(as.Date(s$date))))
  dtn <- unclass(range(diff(as.Date(n$date))))
  if (nrow(s) > 1) expect_gt(min(dts),  27)
  if (nrow(n) > 1) expect_gt(min(dtn),  27)

  expect_true(nrow(a) > (nrow(n) + nrow(s)))
})
