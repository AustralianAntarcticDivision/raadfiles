context("list-data")

test_that("list files found", {
  skip_on_travis()  ## we need bowerbird
  skip_if_not(!is.null(get_raad_data_roots()))
  thelist_files(format = "all")  %>%   expect_named(c("fullname", "root")) ## get all files
  thelist_files(format = "tab", pattern = "tab$")  %>% expect_s3_class("tbl_df") ## those kind
  parcels <- thelist_files(pattern = "parcel")
  cparcels <- thelist_files(pattern = "parcels_c")
  expect_true(nrow(parcels) > nrow(cparcels))
  thelist_files(format = "tab") %>% expect_named(c("fullname", "root"))
})

test_that("read em all", {
  skip_on_travis()
  skip_if_not(!is.null(get_raad_data_roots()))
  files <- thelist_files(format = "gdb", pattern = "transport_segments")

  expect_that(nrow(files), equals(29L))
})

