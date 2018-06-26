### Reproject condition rasters to match Leaflet

library(raster)

infolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Condition Rasters 90m/"   # folder holding original condition rasters
outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Condition Rasters 90m LEAFLET/"   # folder to hold reprojected condition rasters

condition <- stack(list.files(infolder, pattern=glob2rx("*tif$"), full.names=TRUE))  # stack condition rasters
condition.reproj <- projectRaster(condition, crs="+init=epsg:3857", method="ngb")  # reproject to Leaflet EPSG

condition.reproj.unstack <- unstack(condition.reproj)
outputnames <- paste(outfolder, names(condition.reproj), ".tif",sep="")
for(i in seq_along(condition.reproj.unstack)){writeRaster(condition.reproj.unstack[[i]], file=outputnames[i], overwrite=TRUE)}



### Reproject HUC unit rasters to match Leaflet

infolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones/"   # folder holding original zones rasters
outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones LEAFLET/"   # folder to hold reprojected zones rasters

zones10 <- raster(paste0(infolder, "HUC12_zones_10m_NAD83UTM.tif"))  # read in 10-m zones layer
zones10.reproj <- projectRaster(zones10, crs="+init=epsg:3857", method="ngb")  # reproject to Leaflet EPSG

zones30 <- raster(paste0(infolder, "HUC12_zones_30m_NAD83UTM.tif"))  # read in 10-m zones layer
zones30.reproj <- projectRaster(zones30, crs="+init=epsg:3857", method="ngb")  # reproject to Leaflet EPSG

zones90 <- raster(paste0(infolder, "HUC12_zones_90m_NAD83UTM.tif"))  # read in 10-m zones layer
zones90.reproj <- projectRaster(zones90, crs="+init=epsg:3857", method="ngb")  # reproject to Leaflet EPSG

zones270 <- raster(paste0(infolder, "HUC12_zones_270m_NAD83UTM.tif"))  # read in 10-m zones layer
zones270.reproj <- projectRaster(zones270, crs="+init=epsg:3857", method="ngb")  # reproject to Leaflet EPSG

writeRaster(zones90.reproj, paste0(outfolder, "HUC12_zones_90m_NAD83UTM.tif"))
condition.reproj.unstack <- unstack(condition.reproj)
outputnames <- paste(outfolder, names(condition.reproj), ".tif",sep="")
for(i in seq_along(condition.reproj.unstack)){writeRaster(condition.reproj.unstack[[i]], file=outputnames[i])}


