context("list-data")

test_that("list files found", {
  skip_on_travis()  ## we need bowerbird
  thelist_files(format = "")  %>%   expect_named(c("file", "fullname")) ## get all files
  thelist_files(format = "tab", pattern = "tab$")  %>% expect_s3_class("tbl_df") ## those kind
  parcels <- thelist_files(pattern = "parcel")
  cparcels <- thelist_files(pattern = "parcels_c")
  expect_true(nrow(parcels) > nrow(cparcels))
  thelist_files(format = "tab") %>% expect_named(c("file", "fullname"))
})

test_that("read em all", {
  skip_on_travis()
  files <- thelist_files(format = "gdb", pattern = "transport_segments")

  expect_that(nrow(files), equals(29L))
}
)
