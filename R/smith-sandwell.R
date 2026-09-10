.ssbintext <- function() {
  '<VRTDataset rasterXSize="21600" rasterYSize="17280">
  <GeoTransform>0.0, 1855.325, 0.0000000000000000e+000, 15987768, 0.0000000000000000e+000,-1850.436</GeoTransform>
  <SRS>SRS:3857</SRS>
  <Metadata />
  <VRTRasterBand dataType="Int16" band="1" subClass="VRTRawRasterBand">
  <Metadata />
  <SourceFilename relativeToVRT="1">../topo_20.1.img</SourceFilename>
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
  <SRS>EPSG:3857</SRS>
  <GeoTransform>-20037510.000, 1.8553250000000000e+003, 0.0000000000000000e+000, 1.5987768000000000e+007, 0.0000000000000000e+000,-1.8504359999999999e+003</GeoTransform>
  <Metadata />
  <VRTRasterBand dataType="Int16" band="1">
  <Metadata />
  <SimpleSource>
  <SourceFilename relativeToVRT="1">topo_20.1.vrt"</SourceFilename>
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

