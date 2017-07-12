context("list-data")

test_that("list files found", {
  skip_on_travis()  ## we need bowerbird
  thelist_files(format = "")    ## get all files
  thelist_files(pattern = "shp$")  ## those kind
  thelist_files(type = "parcel")
  thelist_files(format = "tab")
})

test_that("read em all", {
  skip_on_travis()
  files <- thelist_files(format = "gdb", type = "transport_segments")

  library(future)
  plan(multiprocess)
  system.time(a <- future_lapply(files$fullname, sf::read_sf))

  library(dplyr)
  b <- do.call(rbind, a)
  ##256255  x   24
  ## 235Mb
  aa <- PRIMITIVE(b)
  library(tabularaster)
  r <- raster::raster(spex::buffer_extent(b, 10), res = 10)
#  cell <- cellnumbers(r, b[1:100, ])
}
)
