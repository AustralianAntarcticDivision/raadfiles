
topo_files_generic <- function(pattern, ...) {
  ## maybe tolower both?
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, pattern))
  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), root = .data$root)
  if (nrow(files) < 1)
    stop("no files found")
  files
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
#' Two versions 2008 and 2014.
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
#' gebco14_files()
gebco19_files <- function(all = FALSE, ...) {

  pattern <- if (all) "www.bodc.ac.uk/.*/GEBCO_15SEC/zip" else "www.bodc.ac.uk/.*/GEBCO_15SEC/zip/GEBCO_2019.nc$"
  topo_files_generic(pattern)
}

#' @name topography
#' @rdname topography-files
#' @export
gebco14_files <- function(all = FALSE, ...) {
  pattern <- if (all) "www.bodc.ac.uk/gebco/" else "www.bodc.ac.uk/.*/GEBCO_2014_2D.nc$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
gebco08_files <- function(all = FALSE, ...) {
  pattern <- if (all) "www.bodc.ac.uk/gebco/" else "www.bodc.ac.uk/.*/GRIDONE_2D.nc$"
  topo_files_generic(pattern)
}

#' @export
#' @name topography
#' @rdname topography-files
ramp_files <- function(all = FALSE, ...) {
  pattern <- if (all) {
    "sidads.colorado.edu/pub/DATASETS/nsidc0082_radarsat_dem_v02/200M/ARCINFO"

    } else {
      "sidads.colorado.edu/pub/DATASETS/nsidc0082_radarsat_dem_v02/200M/ARCINFO/osu91a200m/.*w001001.adf$"

    }
#  print(pattern)
  topo_files_generic(pattern)
}


#' @export
#' @name topography
#' @rdname topography-files
ibcso_files <- function(all = FALSE, ...) {
  pattern <- if (all) "hs.pangaea.de/.*/IBCSO.*" else "hs.pangaea.de/.*/ibcso_v1_is.tif$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
ibcso_background_files <- function(all = FALSE, ...) {
  pattern <- if (all) "hs.pangaea.de/.*/IBCSO.*" else "hs.pangaea.de/.*/ibcso_background_hq.tif$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
ibcso_bed_files <- function(all = FALSE, ...) {
  pattern <- if (all) "hs.pangaea.de/.*/IBCSO.*" else "hs.pangaea.de/.*/ibcso_v1_bed.grd$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
ibcso_digital_chart_files <- function(all = FALSE, ...) {
  pattern <- if (all) "hs.pangaea.de/.*/IBCSO.*" else "hs.pangaea.de/.*/IBCSO_v1_digital_chart_pdfA.pdf$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
ibcso_sid_files <- function(all = FALSE, ...) {
  pattern <- if (all) "hs.pangaea.de/.*/IBCSO.*" else "hs.pangaea.de/.*/ibcso_v1_sid.grd$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
cryosat2_files <- function(all  = FALSE, ...) {
  ## "/Ant": we need to avoid the __MACOSX/._Antar... version
  pattern <- if (all) "earth.esa.int/documents/10174/3082676/" else "earth.esa.int.*/Antarctica_DEM_CryoSat2_v1.2.nc$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
etopo1_files <- function(all = FALSE, ...)
{
  pattern <- if (all) "www.ngdc.noaa.gov/mgg/global/relief/" else "www.ngdc.noaa.gov/.*/ETOPO1_Ice_g_gdal.grd$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
etopo2_files <- function(all = FALSE, ...)
{
  pattern <- if (all) "www.ngdc.noaa.gov/mgg/global/relief/" else "www.ngdc.noaa.gov/.*/ETOPO2v2c_f4.nc$"
  topo_files_generic(pattern)

}
#' @export
#' @name topography
#' @rdname topography-files
lakesuperior_files <- function(all = FALSE, ...)  {
  pattern <- if (all) "www.ngdc.noaa.gov/mgg/greatlakes/superior" else "www.ngdc.noaa.gov/mgg/greatlakes/superior/data/netcdf/superior_lld.grd$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
kerguelen_files <- function(all = FALSE, ...) {
  pattern <- if (all) "ftt.jcu.edu.au" else "ftt.jcu.edu.au/.*/kerg_dem.grd$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
george_v_terre_adelie_1000m_files <- function(all = FALSE, ...) {
  pattern <- if (all) "data.aad.gov.au/eds/file/4494" else "data.aad.gov.au/eds/file/4494/gvdem1000m_v3.nc$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
george_v_terre_adelie_500m_files <- function(all = FALSE, ...) {
  pattern <- if (all) "data.aad.gov.au/eds/file/4494" else "data.aad.gov.au/eds/file/4494/gvdem500m_v3.nc$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
george_v_terre_adelie_250m_files <- function(all = FALSE, ...) {
  pattern <- if (all) "data.aad.gov.au/eds/file/4494" else "data.aad.gov.au/eds/file/4494/gvdem250m_v3.nc$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
george_v_terre_adelie_100m_files <- function(all = FALSE, ...) {
  pattern <- if (all) "data.aad.gov.au/eds/file/4494" else "data.aad.gov.au/eds/file/4494/gvdem100m_v3.nc$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
smith_sandwell_files <- function(all = FALSE, ...) {
  pattern <- if (all) "topex.ucsd.edu/pub/global_topo_1min" else "topo_18.1.vrt$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
smith_sandwell_unpolished_files <- function(all = FALSE, ...) {
  pattern <- if (all) "topex.ucsd.edu/pub/global_topo_1min" else "topo_18.1_unpolished.vrt$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
smith_sandwell_lon180_files <- function(all = FALSE, ...){
  pattern <- if (all) "topex.ucsd.edu/pub/global_topo_1min" else ".vrt/topo_18.1_atlantic.vrt$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
smith_sandwell_unpolished_lon180_files <- function(all = FALSE, ...){
  pattern <- if (all) "topex.ucsd.edu/pub/global_topo_1min" else ".vrt/topo_18.1_unpolishedatlantic.vrt$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
macquarie100m_57S_files <- function(all = FALSE, ...) {
  pattern <- if (all) "www.ga.gov.au/corporate_data/73697/01_ESRI_Raster" else "Macquarie1WGS84UTM57S_100m/macrie1100m/w001001x.adf$"
  topo_files_generic(pattern)
}
#' @export
#' @name topography
#' @rdname topography-files
macquarie100m_58S_files <- function(all = FALSE, ...) {
  pattern <- if (all) "www.ga.gov.au/corporate_data/73697/01_ESRI_Raster" else "Macquarie2WGS84UTM58S_100m/macrie2100m/w001001x.adf$"
  topo_files_generic(pattern)
}
