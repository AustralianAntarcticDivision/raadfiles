## code to prepare `DATASET` dataset goes here

rema_tiles_polygons <- raster::shapefile(rema_tile_files()$fullname)

library(dplyr)
d <- tibble::as_tibble(rema_tiles_polygons@data)
pts <- do.call(rbind, lapply(strsplit(d$tile, "_"), as.integer))[,2:1]
d <- mutate(d, tile_x = pts[,1], tile_y  = pts[,2],
               tile) %>% select(tile, tile_x, tile_y)
rema_tiles_polygons@data <- d
rema_tiles_polygons <- rema_tiles_polygons[order(rema_tiles_polygons$tile_y, rema_tiles_polygons$tile_x), ]

usethis::use_data(rema_tiles_polygons, overwrite = FALSE)
