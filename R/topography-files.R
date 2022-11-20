
topo_files_generic <- function(pattern, ...) {
 .find_files_generic(pattern)
}

#' Topographic data files
#'
#' Obtain file names for various topographic data.
#'
#' Each function exists to match a specific data set, but the optional `all` argument may be used to
#' easily discover a broader set of files that ship with the data, or that represent older versions, documentation
#' and other metadata files.
#'
#' There's no single format, there are GeoTIFFs, ArcInfo binary, ERStorage, NetCDF, NetCDF GMT, (Geo) PDF, and some VRT
#' wrappers for handling raw binary files.
#'
#' @section GEBCO General Bathymetric Chart of the Oceans:
#' Versions 2008, 2014, 2019, 2021.
#' @section IBCSO International Bathymetric Chart of the Southern Ocean:
#' 'is' ('is_PS71' tif, or grd), 'background_hq', 'bed' ('bed_PS71'), 'digital_chart', 'sid' ('sid_PS71')
#' @section ETOPO:
#' Etopo1 and Etopo2, Lake Superior
#' @section Smith and Sandwell:
#' Polished and unpolished, version 18.1  replaces 18.
#' @section Cryosat2:
#' Cryosat2
#' @section Australian Antarctic:
#' George V Terre Adelie, Kerguelen, Macquarie 100m
#' @param all return a larger set of files (for exploratory use only)
#' @param ... reserved
#' @name topography
#' @rdname topography-files
#' @return data frame of `file` and `fullname` columns
#' @export
#'
#' @examples
#' \dontrun{
#'   gebco21_files()
#' }
#'
gebco21_files <- function(all = FALSE, ...) {
 if (all) {
    out <- topo_files_generic("www.bodc.ac.uk/.*gebco/gebco_2021.*")
  } else {
    out <- topo_files_generic(c("gebco", "gebco/GEBCO_2021.tif"))
  }
  out
}
#' @name topography
#' @rdname topography-files
#' @export
gebco19_files <- function(all = FALSE, ...) {

if (all) {
    out <- topo_files_generic("www.bodc.ac.uk/.*/GEBCO_15SEC.*")
  } else {

    pattern <- c("data_local", "aad.gov.au/gebco/GEBCO_2019.tif")
    out <- .find_files_generic(pattern)
  }
  out
}

#' @name topography
#' @rdname topography-files
#' @export
gebco14_files <- function(all = FALSE, ...) {
  pattern <- "www.bodc.ac.uk/gebco/"
  if (!all) pattern <- c(pattern, "www.bodc.ac.uk/.*/GEBCO_2014_2D.nc$")
  out <- topo_files_generic(pattern)
  if (!all) out <- out[1L, , drop = FALSE]
  out
}
#' @export
#' @name topography
#' @rdname topography-files
gebco08_files <- function(all = FALSE, ...) {
  pattern <- "www.bodc.ac.uk/gebco/"
  if (!all) pattern <- c(pattern, "www.bodc.ac.uk/.*/GRIDONE_2D.nc$")
 out <- topo_files_generic(pattern)
  if (!all) out <- out[1L, , drop = FALSE]
 out
}

#' @export
#' @name topography
#' @rdname topography-files
ramp_files <- function(all = FALSE, ...) {
  pattern <- "sidads.colorado.edu/pub/DATASETS/nsidc0082_radarsat_dem_v02/200M/ARCINFO"
  if (!all) pattern <- c(pattern,"sidads.colorado.edu/pub/DATASETS/nsidc0082_radarsat_dem_v02/200M/ARCINFO/osu91a200m/.*w001001.adf$")
#  print(pattern)
  topo_files_generic(pattern)
}


#' @export
#' @name topography
#' @rdname topography-files
ibcso_files <- function(all = FALSE, ...) {
                       ##download.pangaea.de/dataset/937574/files/IBCSO_v2_ice-surface.tif
  pattern <- "download.pangaea.de/dataset/937574/files/.*"
  if (!all) pattern <- c(pattern, ".*IBCSO_v2_ice-surface.tif$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
ibcso_background_files <- function(all = FALSE, ...) {
  pattern <- "hs.pangaea.de/.*/IBCSO.*"
  if (!all) pattern <- c(pattern, "hs.pangaea.de/.*/ibcso_background_hq.tif$")
  message("'background' is currently IBCSO v1 (see other 'ibcso_*_files' functions for v2")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
ibcso_bed_files <- function(all = FALSE, ...) {
  pattern <- "download.pangaea.de/dataset/937574/files/.*"
  if (!all) pattern <- c(pattern, "download.pangaea.de/dataset/937574/files/IBCSO_v2_bed.tif")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
ibcso_digital_chart_files <- function(all = FALSE, ...) {
  pattern <- "download.pangaea.de/dataset/937574/files/.*"
  if (!all) pattern <- c(pattern, "download.pangaea.de/dataset/937574/files/IBSCO_v2_digital_chart.pdf$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
ibcso_rid_files <- function(all = FALSE, ...) {
  pattern <- "download.pangaea.de/dataset/937574/files/.*"
  if (!all) pattern <- c(pattern, "download.pangaea.de/dataset/937574/files/IBCSO_v2_RID.tif$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
ibcso_tid_files <- function(all = FALSE, ...) {
  pattern <- "download.pangaea.de/dataset/937574/files/.*"
  if (!all) pattern <- c(pattern, "download.pangaea.de/dataset/937574/files/IBCSO_v2_TID.tif$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
ibcso_sid_files <- function(all = FALSE, ...) {
  pattern <- "hs.pangaea.de/.*/IBCSO.*"
  if (!all) pattern <- c(pattern, "hs.pangaea.de/.*/ibcso_v1_sid.grd$")
  message("'sid' is currently IBCSO v1 (see other 'ibcso_*_files' ('rid', 'tid') functions for v2")

  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
cryosat2_files <- function(all  = FALSE, ...) {
  ## "/Ant": we need to avoid the __MACOSX/._Antar... version
  pattern <- "earth.esa.int/documents/10174/3082676/"
  if (!all) pattern <- c(pattern, "earth.esa.int.*/Antarctica_DEM_CryoSat2_v1.2.nc$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
etopo1_files <- function(all = FALSE, ...)
{
  pattern <- "www.ngdc.noaa.gov/mgg/global/relief/"
  if (!all) pattern <- c(pattern, "www.ngdc.noaa.gov/.*/ETOPO1_Ice_g_gdal.grd$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
etopo2_files <- function(all = FALSE, ...)
{
  pattern <- "www.ngdc.noaa.gov/mgg/global/relief/"
  if (!all) pattern <- c(pattern, "www.ngdc.noaa.gov/.*/ETOPO2v2c_f4.nc$")
  topo_files_generic(pattern)

}
#' @export
#' @name topography
#' @rdname topography-files
lakesuperior_files <- function(all = FALSE, ...)  {
  pattern <- "www.ngdc.noaa.gov/mgg/greatlakes/superior"
  if (!all) pattern <- c(pattern, "www.ngdc.noaa.gov/mgg/greatlakes/superior/data/netcdf/superior_lld.grd$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
kerguelen_files <- function(all = FALSE, ...) {
  ## updated 2021-03-01
  #pattern <- if (all) "ftt.jcu.edu.au" else "ftt.jcu.edu.au/.*/kerg_dem.grd$"

    pattern <- "d28rz98at9flks.cloudfront.net/71552"
    if (!all) pattern <- c(pattern,  "d28rz98at9flks.cloudfront.net/71552/.*kerg100_28mar/w001001.adf")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
george_v_terre_adelie_1000m_files <- function(all = FALSE, ...) {
  pattern <- "data.aad.gov.au/eds/file/4494"
  if (!all) pattern <- c(pattern, "data.aad.gov.au/eds/file/4494/gvdem1000m_v3.nc$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
george_v_terre_adelie_500m_files <- function(all = FALSE, ...) {
  pattern <- "data.aad.gov.au/eds/file/4494"
  if (!all) pattern <- c(pattern, "data.aad.gov.au/eds/file/4494/gvdem500m_v3.nc$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
george_v_terre_adelie_250m_files <- function(all = FALSE, ...) {
  pattern <- "data.aad.gov.au/eds/file/4494"
  if (!all) pattern <- c(pattern, "data.aad.gov.au/eds/file/4494/gvdem250m_v3.nc$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
george_v_terre_adelie_100m_files <- function(all = FALSE, ...) {
  pattern <- "data.aad.gov.au/eds/file/4494"
  if (!all) pattern <- c(pattern, "data.aad.gov.au/eds/file/4494/gvdem100m_v3.nc$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
smith_sandwell_files <- function(all = FALSE, ...) {
  pattern <- "topex.ucsd.edu/pub/global_topo_1min"
  if (!all) pattern <- c(pattern, "topo_18.1.vrt$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
smith_sandwell_unpolished_files <- function(all = FALSE, ...) {
  pattern <- "topex.ucsd.edu/pub/global_topo_1min"
  if (!all) pattern <- c(pattern, "topo_18.1_unpolished.vrt$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
smith_sandwell_lon180_files <- function(all = FALSE, ...){
  pattern <- "topex.ucsd.edu/pub/global_topo_1min"
  if (!all) pattern <- c(pattern, ".vrt/topo_18.1_atlantic.vrt$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
smith_sandwell_unpolished_lon180_files <- function(all = FALSE, ...){
  pattern <- "topex.ucsd.edu/pub/global_topo_1min"
  if (!all) pattern <- c(pattern, ".vrt/topo_18.1_unpolishedatlantic.vrt$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
macquarie100m_57S_files <- function(all = FALSE, ...) {
  pattern <- "www.ga.gov.au/corporate_data/73697/01_ESRI_Raster"
  if (!all) pattern <- c(pattern, "Macquarie1WGS84UTM57S_100m/macrie1100m/w001001x.adf$")
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
macquarie100m_58S_files <- function(all = FALSE, ...) {
  pattern <- "www.ga.gov.au/corporate_data/73697/01_ESRI_Raster"
  if (!all) pattern <- c(pattern, "Macquarie2WGS84UTM58S_100m/macrie2100m/w001001x.adf$")
  topo_files_generic(pattern)
}
