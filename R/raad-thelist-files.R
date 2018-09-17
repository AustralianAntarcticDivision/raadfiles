#' TheList
#'
#' Local authority spatial data in Tasmania.
#'
#' TheList.tas.gov.au is the standard local mapping authority in Tasmania, it was
#' recently upgraded and works very well, and most of the data including newish
#' LiDAR is available within the opendata tree. Also check out tasmap.org for an
#' alternative interface with access to the same services.
#'
#' These files are broken into sub-regions, administrative areas within the
#' state of Tasmania. At time of checking there were 19 sub-regions, and 544 or so
#' layers (type within format) and 37,616 total files. GDB detection is different to the other
#' more definite formats so the file sets won't be analogous atm.  There are Climate
#' Futures Australia (CFA) layer indexes in here as well, it's on the todo list
#' to build a comprehensive index (or access one).
#'
#' The scheme uses the Map Grid of Australia 1994 (MGA94) on
#' the Geocentric Datum of Australia 1994 (GDA94), an implementation of UTM Zone
#' 55. GDA94 was rolled out in Australia in the early 2000s, Tasmania kept the
#' old UTM scheme (it was AMG66, AGD66) but around the same time Victoria used
#' the opportunity to move to a single-projection for the entire state, to avoid
#' having to switch between zones. NSW took much longer to modernize and
#' standardize around GDA94 and they stumbled forward with their three UTM zones
#' (54, 55, 56), and while Tasmania did it quickly we only have the one zone (no
#' one thought much about Macquarie Island) and Victoria did it more cleverly.
#' I'm not sure how Queensland went, they were adding properties and roads at a
#' very scary rate so probably took much longer than us. The software back then
#' could only just handle an entire city worth of vector roads and cadastre, so
#' experience with higher abstractions was pretty rare. As a nation, we could
#' probably never have agreed on a national LCC projection given that the
#' states had so much mess to sort out themselves, but that's what you see in
#' the USA with its Albers family, and the elided Hawaii and and Alaskan
#' montsrosities . During the time GDA94 was being rolled out the addressing
#' system was being standardized for GPS and modern communication systems, the
#' P-NAF was the original project that took the data from Australia Post. State
#' of the art for address parsing in 2002 was Perl, Google Earth was but a
#' keyhole glimmering in the background in early West Wing episodes, and the
#' idea of "micro-services" was catching on among the venture capital elite. Today
#' the echoes of Oracle and ESRI and RP-Data and ENVI and are still with us.
#'
#' It was around this time that the large players made their mark in Australia
#' (mid-1990s-early 2000s), MapInfo had a tight hold on many local government
#' authorities (LGAs) because they had the best software, the best formats (TAB,
#' MIF and georeferenced tile TAB for imagery), and somehow got integrated into
#' many state departments. That's why these TAB and MIF formats are here still,
#' shp was the poor interchange cousin, limited to DBF, short field names, no
#' data above the 32-bit index limit, no mixed topologies in a single layer.
#' Aerial imagery was just starting to make an impact and the future business
#' and research interests being recognized. MrSID and ECW were used to integrate
#' large imagery collections into single files and services, while their parent
#' companies waged a furious legal battle around wavelet compression. LizardTech
#' has mostly faded from the scene, but NearMap continues today with "reality as
#' a service", they certainly had the long-game in mind this whole time.
#'
#' Manifold was in version 5.0 in 2002, and it could read all of these formats
#' as well as provide very accessible rendering, ability to create tiles with
#' links betweeen drawings and images for creating tile sets. ECW was absolutely
#' hot, and ERMapper (Nearmap) had a free viewer that is probably still the best
#' one around until leaflet happened. The point of this long story was to
#' explain that in the early 2000s these files were LARGE and no one had a hope
#' of reading a road line, cadastral parcel, or even address point shapefile for
#' an entire state. We read them in parts, and in pairs or more of parts while
#' we slowly rendered our way around the country building map tile sets
#' deprecated immediately when Google Earth arrived. These days it's a pain to
#' get the file list into one object so you lapply it with the latest R GIS i/o
#' package, but there's really no problem with memory.
#'
#' This function is here to make it easy to get the file list for Tasmania.
#'
#' tab, gdb, shp is sf/rgdal ready - gdb works with the directory name, it might
#' work with some of the individual files - I don't know how GDAL otherwise
#' resolves layer names in the same folder but you can give it the path name,
#' this is probably why gdb/, though note that for raster /anything-next-to.adf
#' does work
#'
#' tab is that glorious ancient format
#'
#' gdb is a newcomer format, recently reverse engineered by Even
#'
#' shp is the usual suspect
#'
#' dbf is the usual suspect's data (ggplot2 calls this metadata)
#'
#' asc is DEM e.g. list_dem_25m_break_o_day.asc (part of a statewide effort in the
#' early 2000s to build a DEM for Tasmania, it was used to build a networked drainage
#' and topography graph of the state's physical landscape, and this helped spur the
#' development of a powerful imagery orthorectification system and led to some
#' interesting commerical initiatives in general geospatial data)
#'
#' csv is something else e.g. list_fmp_data.csv
#'
#' txt is probably just xml, probably only relevant to GDAL and ESRI  list_fmp_data_statewide.txt.xml
#'
#' Arguments are used to pattern match on different aspects of the file name so that anything can be pulled out.
#' @param format is used to targe tspecific formats see Details
#' @param pattern is used to string match generally, if this is not NULL then format is ignored
#'
#' @return tibble data frame of file names
#' @importFrom dplyr %>% arrange distinct filter mutate
#' @importFrom rlang .data
#' @importFrom stringr str_detect str_replace
#' @name thelist
#' @export

#' @examples
#' thelist_files()
thelist_files <- function(format = c("gdb", "tab", "shp", "asc"),
                          pattern = NULL
) {
  files <- dplyr::filter(get_raw_raad_filenames(), stringr::str_detect(.data$file, "listdata.thelist.tas.gov.au"))
  # unique(unlist(lapply(strsplit(files$file, "\\."), tail, 1)))
  # [1] "cpg"            "DAT"            "dbf"            "gdbindexes"     "gdbtable"       "gdbtablx"
  # [7] "atx"            "freelist"       "spx"            "gdb/gdb"        "gdb/timestamps" "ID"
  # [13] "MAP"            "prj"            "sbn"            "sbx"            "shp"            "xml"
  # [19] "shx"            "tab"            "zip"            "log"            "html"           "html@C=M;O=A"
  # [25] "html@C=M;O=D"   "html@C=N;O=A"   "html@C=N;O=D"   "html@C=S;O=A"   "html@C=S;O=D"   "asc"
  # [31] "csv"            "txt"            "lyr"
  #
  ## what will sf read
  ## dirname(<gdbindexes>) is fine
  #  afile <- files %>% filter(grepl("gdbindexes", file)) %>% slice(1) %>% mutate(fullname = file.path(root, file)) %>% pull(fullname)
  #  sf::read_sf(dirname(afile))
  #  afile <- files %>% filter(grepl("asc", file)) %>% slice(1) %>% mutate(fullname = file.path(root, file)) %>% pull(fullname)


  #if (!is.null(type)) files <- dplyr::filter(files, grepl(type, file))
  if (!is.null(pattern)) files <- dplyr::filter(files, grepl(pattern, .data$file))
  if (is.null(format) || is.na(format) || nchar(format) == 0) {

  } else {
    format <- match.arg(format)
    if (format == "gdb") {
      files <- dplyr::filter(files, grepl("gdb$", dirname(.data$file)))
      files <- dplyr::mutate(files, file = dirname(.data$file)) %>% distinct(.data$root, .data$file)
    } else {
      files <- dplyr::filter(files, grepl(sprintf("%s$", format), .data$file))
    }
  }


  #datadir <- get_raad_datadir()
  #if (!datadir == files$root[1]) warning("datadir and file root don't match?")
  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), root = .data$root)

  if (nrow(files) < 1)
    stop("no files found")

  #files <- dplyr::mutate(files,
   #                      file = stringr::str_replace(.data$fullname, paste0(datadir, "/"), ""))
  dplyr::arrange(files)
}
