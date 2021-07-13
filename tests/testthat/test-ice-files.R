context("ice-files")

test_that("file stamps are sensible", {
  skip_if_not(any(nzchar(raadfiles::get_raad_data_roots())))
  s <- nsidc_south_monthly_files()
  n <- nsidc_north_monthly_files()
  a <- nsidc_monthly_files()
  dts <- unclass(range(diff(as.Date(s$date))))
  dtn <- unclass(range(diff(as.Date(n$date))))
  if (nrow(s) > 1) expect_gt(min(dts),  27)
  if (nrow(n) > 1) expect_gt(min(dtn),  27)

  expect_true(nrow(a) > (nrow(n) + nrow(s)))
})
