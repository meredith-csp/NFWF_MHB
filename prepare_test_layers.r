# Create a series of test layers (mask, target, HUC12) that are spatially aligned (projection, origin, resolution)

library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(rvest)
library(maptools)
library(ggplot2)



outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Test Layers/"

### Load shapefiles
test.mask <- raster("C:/Work/SpatialData/NFWF_Cross_Realm/Mask layers/conservation_easement_mask.tif")
test.target1 <- raster("C:/Work/SpatialData/NFWF_Cross_Realm/Theobald_flowlines_clipped/Theobald_forest_flowlines.tif") 
test.target2 <- raster("C:/Work/SpatialData/NFWF_Cross_Realm/summer_streamflow_2040_percent_change.tif")
test.huc12 <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/RasterTemplate/mhb_plu_270.tif")

### Reproject to common projection - use UTM Zone 12N, NAD83
newproj <- proj4string(test.huc12)
test.mask.prj <- projectRaster(test.mask, crs=newproj, method="ngb") # use nearest neighbor for categorical raster
test.target1.prj <- projectRaster(test.target1, crs=newproj, method="bilinear")  # use bilinear interpolation for continuous raster
test.target2.prj <- projectRaster(test.target2, crs=newproj, method="bilinear")  # use bilinear interpolation for continuous raster

### Resample to origin and resolution of HUC12 layer
test.mask.resamp <- resample(test.mask.prj, test.huc12, method="ngb")
test.target1.resamp <- resample(test.target1.prj, test.huc12, method="bilinear")
test.target2.resamp <- resample(test.target2.prj, test.huc12, method="bilinear")

### Write outputs to Google Drive
writeRaster(test.mask.resamp, paste0(outfolder,"test_easement_mask.tif"))
writeRaster(test.target1.resamp, paste0(outfolder,"test_forest_connectivity.tif"))
writeRaster(test.target2.resamp, paste0(outfolder,"test_streamflow_percent_change.tif"))
writeRaster(test.huc12, paste0(outfolder,"test_huc12.tif"))
