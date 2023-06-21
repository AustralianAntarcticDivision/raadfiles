# File ftp-access.aviso.altimetry.fr/duacs-experimental/dt-phy-grids/altimetry_antarctic/version_01_00/dt_antarctic_multimission_sea_level_uv_20130401_20190731.nc (NC_FORMAT_NETCDF4):
#
#   6 variables (excluding dimension variables):
#   float latitude[y,x]   (Contiguous storage)
# axis: Y
# long_name: Latitude
# standard_name: latitude
# units: degrees_north
# valid_min: -89.8417311687249
# valid_max: -32.1972744948259
# float longitude[y,x]   (Contiguous storage)
# axis: X
# long_name: Longitude
# standard_name: longitude
# units: degrees_east
# valid_min: -179.835829161283
# valid_max: 179.835829161283
# float sla[y,x,time]   (Contiguous storage)
# long_name: Sea Level Anomaly
# coordinates: time u v
# standard_name: sea_surface_height_above_sea_level
# units: m
# float formal_error[y,x,time]   (Contiguous storage)
# long_name: Optimal Interpolation Formal Error
# coordinates: time u v
# standard_name: formal_err
# units: m
# float U[y,x,time]   (Contiguous storage)
# long_name: Zonal Geostrophic Current Anomaly
# coordinates: time u v
# standard_name: zonal_geostophic_current_anomaly
# units: m/s
# float V[y,x,time]   (Contiguous storage)
# long_name: Meridional Geostrophic Current Anomaly
# coordinates: time u v
# standard_name: meridional_geostophic_current_anomaly
# units: m/s
#
# 3 dimensions:
#   time  Size:2313
# axis: T
# calendar: gregorian
# long_name: Time
# standard_name: time
# units: days since 1950-01-01 00:00:00
# x  Size:350
# [1] "vobjtovarid4: **** WARNING **** I was asked to get a varid for dimension named x BUT this dimension HAS NO DIMVAR! Code will probably fail at this point"
# y  Size:350
# [1] "vobjtovarid4: **** WARNING **** I was asked to get a varid for dimension named y BUT this dimension HAS NO DIMVAR! Code will probably fail at this point"
#
# 35 global attributes:
#   Conventions: CF-1.7
# Metadata_Conventions: Unidata Dataset Discovery v1.0
# cdm_data_type: Grid
# comment: Sea Level Anomaly measured by Altimetry and derived variables
# contact: aviso@altimetry.fr
# creator_email: aviso@altimetry.fr
# creator_name: ANTARCTIC_OCEAN_PROTOTYPE
# creator_url: https://www.aviso.altimetry.fr
# geospatial_lat_max: -32.1972744948259
# geospatial_lat_min: -89.8417311687249
# geospatial_lat_units: degrees_north
# geospatial_lon_max: 179.835829161283
# geospatial_lon_min: -179.835829161283
# geospatial_lon_units: degrees_east
# geospatial_vertical_max: 0
# geospatial_vertical_min: 0
# geospatial_vertical_positive: down
# geospatial_vertical_resolution: point
# geospatial_vertical_units: m
# institution: CLS,CNES
# keywords: Oceans>Ocean Topography>Sea Surface Height
# keywords_vocabulary: NetCDF COARDS Climate and Forecast Standard Names
# platform: AL_C2_S3A
# processing_level: L4
# Grid: Subset of Southern Hemisphere 25km EASE2 Grid
# title: DT multi-satellite sea level gridded product
# product_version: 1.1
# reference: http://aviso.altimetry.fr
# source: Altimetry measurements
# standard_name_vocabulary: NetCDF Climate and Forecast (CF) Metadata Convention Standard Name Table v37
# time_coverage_duration: P2312.0D
# time_coverage_resolution: P1.0D
# time_coverage_end: 2019-07-31 00:00:00Z
# time_coverage_start: 2013-04-01 00:00:00Z
# history: Tue May 18 14:42:38 2021: ncrename -d v,y dt_antarctic_multimission_sea_level_uv_20130401_20193107.nc
# Tue May 18 14:42:29 2021: ncrename -d u,x dt_antarctic_multimission_sea_level_uv_20130401_20193107.nc
# Created on 2021-05-10 14:26:13Z by ANTARCTIC_OCEAN_PROTOTYPE


#' Antarctic Polar Altimetry files
#'
#' Sea Level Anomaly measured by Altimetry and derived variables
#'
#' sla, formal_error, U, V
#'
#' @return data frame of file paths
#' @export
#'
#' @examples
#' \dontrun{
#'   aaf <- altimetry_antarctica_files()
#' }
altimetry_antarctica_files <- function() {
#  ftp-access.aviso.altimetry.fr/duacs-experimental/dt-phy-grids/altimetry_antarctic
pattern <- c("ftp-access.aviso.altimetry.fr",
               "duacs-experimental/dt-phy-grids/altimetry_antarctic.*nc"
               )
  files <- .find_files_generic(pattern)
  if (nrow(files) < 1)
    stop("no files found")
  ## datadir <- get_raad_datadir()
  files <- dplyr::mutate(files, date = as.POSIXct(as.Date(stringr::str_extract(basename(.data$fullname), "[0-9]{8}"),
                                                          "%Y%m%d"),tz = "UTC"))
  dplyr::arrange(dplyr::distinct(files, date, .keep_all = TRUE), date)  %>%
    dplyr::select(.data$date, .data$fullname, .data$root) %>%
    set_dt_utc()

}
