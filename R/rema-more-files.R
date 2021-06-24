# REMA

## ALL files

## RASTER FILES

#file paths

## TILE FILES

#actual polygons, with every tile name, ready for join with specific res, variables

## FULL TILING

# the xy grid of the tile name, in its entirety with proper offset/scale/crs

# .rema_all_files <- function() {
#   ## every file in the tree
# }

.rema_proc_all_files <- function() {
  files <- dplyr::filter(get_raad_filenames(all = TRUE), stringr::str_detect(.data$file,
                                                                            "aad.gov.au/rema/processing"))
  files <- dplyr::transmute(files,fullname = file.path(.data$root, .data$file), root = .data$root)
  files

}

#' @name rema-files
#' @export
rema_8m_tiles <- function() {
  ## polygons
  rema_tiles_polygons
}
#' @name rema-files
#' @export
rema_200m_dem_files <- function() {
  rema_200m_files()
}

#' @name rema-files
#' @export
.rema_file_filter <- function(x) {
  dplyr::filter(.rema_proc_all_files(), stringr::str_detect(.data$fullname, x))
}

#' @name rema-files
#' @export
rema_200m_dem_geoid_files <- function() {
  pat <- ".*200m_filled_geoid.*tif$"
  .rema_file_filter(pat)

}
#' @name rema-files
#' @export
rema_200m_slope_files <- function() {
 pat <- ".*/200m/.*200m_slope.*tif$"
 .rema_file_filter(pat)

}
#' @name rema-files
#' @export
rema_200m_aspect_files <- function() {
  pat <- ".*/200m/.*200m_aspect.*tif$"
  .rema_file_filter(pat)
}
#' @name rema-files
#' @export
rema_200m_rugosity_files <- function() {
  pat <- ".*/200m/.*200m_rugosity.*tif$"
  .rema_file_filter(pat)
}

#' @name rema-files
#' @export
rema_200m_rock_files <- function() {
  pat <- ".*/200m/.*200m_rock.*tif$"
  .rema_file_filter(pat)
}

#' @name rema-files
#' @export
rema_100m_dem_files <- function() {
  rema_100m_files()
}

#' @name rema-files
#' @export
rema_100m_dem_geoid_files <- function() {
  pat <- ".*/100m/.*100m_filled_geoid.*tif$"
  .rema_file_filter(pat)

}

#' @name rema-files
#' @export
rema_100m_slope_files <- function() {
  pat <- ".*/100m/.*100m_slope.*tif$"
  .rema_file_filter(pat)
}
#' @name rema-files
#' @export
rema_100m_aspect_files <- function() {
  pat <- ".*/100m/.*100m_aspect.*tif$"
  .rema_file_filter(pat)
}

#' @name rema-files
#' @export
rema_100m_rugosity_files <- function() {
  pat <- ".*/100m/.*100m_rugosity.*tif$"
  .rema_file_filter(pat)
}


#' @name rema-files
#' @export
rema_100m_rock_files <- function() {
  pat <- ".*/100m/.*100m_rock.*tif$"
  .rema_file_filter(pat)
}
#' @name rema-files
#' @export
rema_8m_dem_files <- function() {
  rema_8m_files()
}

# "/rdsi/PRIVATE/raad2/data_local/aad.gov.au/rema/processing/v1.1/8m/09_38/09_38_8m_filled_geoid.tif"
#' @name rema-files
#' @export
rema_8m_dem_geoid_files <- function() {
  pat <- ".*/8m/.*8m_filled_geoid.*tif$"
  .rema_file_filter(pat)

}

#' @name rema-files
#' @export
rema_8m_slope_files <- function() {
  pat <- ".*/8m/.*8m_slope.*tif$"
  .rema_file_filter(pat)
}
#' @name rema-files
#' @export
rema_8m_aspect_files <- function() {
  pat <- ".*/8m/.*8m_aspect.*tif$"
  .rema_file_filter(pat)
}
#' @name rema-files
#' @export
rema_8m_rugosity_files <- function() {
  pat <- ".*/8m/.*8m_rugosity.*tif$"
  .rema_file_filter(pat)
}
#' @name rema-files
#' @export
rema_8m_rock_files <- function() {
  pat <- ".*/8m/.*8m_rock.*tif$"
  .rema_file_filter(pat)
}

#
#
#
#
#
# ## RAADTOOLS
# rema_200m_dem <- function() {
#   ## original tif
# }
#
# rema_200m_dem_geoid <- function() {
#   ## VRT of the tiles
# }
#
# rema_200m_slope <- function() {
#   ## VRT of the tiles
# }
#




