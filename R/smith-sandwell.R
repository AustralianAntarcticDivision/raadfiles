.ssbintext <- function() {
  '<VRTDataset rasterXSize="21600" rasterYSize="17280">
  <GeoTransform>0.0, 1855.325, 0.0000000000000000e+000, 15987768, 0.0000000000000000e+000,-1850.436</GeoTransform>
  <SRS>PROJCS["unnamed",
  GEOGCS["Normal Sphere (r=6370997)",
  DATUM["unknown",
  SPHEROID["sphere",6370997,0]],
  PRIMEM["Greenwich",0],
  UNIT["degree",0.0174532925199433]],
  PROJECTION["Mercator_1SP"],
  PARAMETER["central_meridian",0],
  PARAMETER["scale_factor",1],
  PARAMETER["false_easting",0],
  PARAMETER["false_northing",0]]</SRS>
  <Metadata />
  <VRTRasterBand dataType="Int16" band="1" subClass="VRTRawRasterBand">
  <Metadata />
  <SourceFilename relativeToVRT="1">ssbinfile</SourceFilename>
  <SourceBand>1</SourceBand>
  <ImageOffset>0</ImageOffset>
  <PixelOffset>2</PixelOffset>
  <LineOffset>43200</LineOffset>
  <ByteOrder>MSB</ByteOrder>
  </VRTRasterBand>
  </VRTDataset>'
}

.ssatlanticbintext <- function() {
  '<VRTDataset rasterXSize="21600" rasterYSize="17280">
  <SRS>PROJCS["unnamed",
  GEOGCS["Normal Sphere (r=6370997)",
  DATUM["unknown",
  SPHEROID["sphere",6370997,0]],
  PRIMEM["Greenwich",0],
  UNIT["degree",0.0174532925199433]],
  PROJECTION["Mercator_1SP"],
  PARAMETER["central_meridian",0],
  PARAMETER["scale_factor",1],
  PARAMETER["false_easting",0],
  PARAMETER["false_northing",0]]</SRS>
  <GeoTransform>-20037510.000, 1.8553250000000000e+003, 0.0000000000000000e+000, 1.5987768000000000e+007, 0.0000000000000000e+000,-1.8504359999999999e+003</GeoTransform>
  <Metadata />
  <VRTRasterBand dataType="Int16" band="1">
  <Metadata />
  <SimpleSource>
  <SourceFilename relativeToVRT="1">ssvrtfile</SourceFilename>
  <SourceBand>1</SourceBand>
  <SourceProperties RasterXSize="21600" RasterYSize="17280" DataType="Int16" BlockXSize="21600" BlockYSize="1" />
  <SrcRect xOff="10800" yOff="0" xSize="10800" ySize="17280" />
  <DstRect xOff="0" yOff="0" xSize="10800" ySize="17280" />
  </SimpleSource>
  <SimpleSource>
  <SourceFilename relativeToVRT="1">ssvrtfile</SourceFilename>
  <SourceBand>1</SourceBand>
  <SourceProperties RasterXSize="21600" RasterYSize="17280" DataType="Int16" BlockXSize="21600" BlockYSize="1" />
  <SrcRect xOff="0" yOff="0" xSize="10800" ySize="17280" />
  <DstRect xOff="10800" yOff="0" xSize="10800" ySize="17280" />
  </SimpleSource>
  </VRTRasterBand>
  </VRTDataset>'
}


.smithsandwellraw <- function(candidatefiles) {
  tail(sort( grep("/topo_.*\\.img$", candidatefiles, value = TRUE)), 1)
}

.smithsandwellvrt <- function(lon180 = FALSE) {
  all_files <- raadfiles:::get_raw_raad_filenames() %>% mutate(fullname = file.path(root, file)) %>% dplyr::pull(fullname)
  binfile <- .smithsandwellraw(all_files)
  vrtfile1 <- file.path(dirname(binfile), ".vrt", gsub(".img$", ".vrt", basename(binfile)))
  vrtfile2 <- file.path(dirname(binfile), ".vrt",gsub(".img$", "atlantic.vrt", basename(binfile)))

  vrtext1 <- gsub("ssbinfile", file.path("..", basename(binfile)), .ssbintext())
  vrtext2 <- gsub("ssvrtfile", basename(vrtfile1), .ssatlanticbintext())

  if (!file.exists(vrtfile1)) writeLines(vrtext1, vrtfile1)
  if (!file.exists(vrtfile2)) writeLines(vrtext2, vrtfile2)
  if (lon180) vrtfile2 else vrtfile1
}
