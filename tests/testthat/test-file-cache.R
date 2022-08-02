context("file cache")
test_that("raadtools config works", {
  #skip_if_not(Sys.info()[["nodename"]] == "raadsync2")
  #options("raadfiles.data.roots" = NULL)
  skip_if_not(!is.null(get_raad_data_roots()))
  skip_if_not(!is.null(getOption("raadfiles.database.status")))
  skip_on_ci()
  # oldpath <- getOption("default.datadir")
  # expect_true(file.exists(oldpath))

  db <- get_raad_filenames()
  db %>% expect_s3_class("tbl_df") %>% expect_named(c("root", "file"))

  cfapath <- getOption("cfafiles.default.data.directory")
  expect_true(is.null(cfapath))

  cfadb <- getOption("cfafiles.filename.database")
  expect_true(is.null(cfadb))

})


