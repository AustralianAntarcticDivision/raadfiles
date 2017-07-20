context("ncep2")

test_that("NCEP2 files good", {
 uwnd <- ncep2_uwnd_6hr_files()
 uwnd %>% expect_s3_class("tbl_df") %>% expect_named(c("file", "fullname", "date"))
 expect_true(inherits(uwnd$date, "POSIXct"))
 expect_true(all(grepl("uwnd", basename(uwnd$file))))
 vwnd <- ncep2_vwnd_6hr_files()
 expect_true(all(grepl("vwnd", basename(vwnd$file))))

})
