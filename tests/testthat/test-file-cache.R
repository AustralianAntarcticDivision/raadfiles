context("file cache")
test_that("raadtools config works", {
  skip_if_not(Sys.info()[["nodename"]] == "raadsync2")
  skip_if_not(any(nzchar(raadfiles::get_raad_data_roots())))
  # oldpath <- getOption("default.datadir")
  # expect_true(file.exists(oldpath))

  db <- getOption("raadfiles.filename.database")
  db %>% expect_s3_class("tbl_df") %>% expect_named(c("root", "file"))

  cfapath <- getOption("cfafiles.default.data.directory")
  expect_true(is.null(cfapath))

  cfadb <- getOption("cfafiles.filename.database")
  expect_true(is.null(cfadb))

})


