By GBIG / Tuuli Toivonen 2013-08-10, Edited 2015-07-27

# landmask.tif
## Description
Data delineating land (pixel value 1) and sea (pixel value 0) areas in the
Helsinki-Uusimaa region. To be used as a mask to limit Zonation analyses to land
areas (=1) only.

Source Data: Corine Land Cover 25 meter raster  
Source Data Provider: Finnish Environment Institute, SYKE  
Resolution: 100 m  
Format: GeoTIFF  
CRS: ETRS89 / ETRS-TM35FIN (EPSG:3067)
Pixel values:  
  0 = sea  
	1 = land

## Production
`landmask.tif` has been created using Corine Landcover data. All pixels having any
land in the original 25 meter Corine data were classified as land.

1. Corine Land Cover data was projected to EUREF coordinate system.
2. Sea areas were extracted from Corine Land Cover (pixel values = 44) and coded
as 0 and others as 1.
3. Original 25 meter data was aggregated in AgrGIS using AGGREGATE to 100 meter
pixels (cell factor 4, SUM).
4. Pixels having any land (values 1 to 16) were classified as land using
Reclassify.  

## Provenance information
- `landmask_100m.tif` compressed and renamed to `landmask.tif` (2015-07-16)
