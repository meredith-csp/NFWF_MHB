library(raster)

# First, need to add new mask layers for woody encroachment (3 different masks)

encroach <- raster("C:/Work/SpatialData/NFWF_Cross_Realm/Final Input Rasters/grass_and_shrub_encroach_500m_buffer_10m_NAD83UTM.tif")
encroach.90m <- raster::aggregate(encroach, fact=9, fun="modal")

zones.10m <- raster::disaggregate(zones, fact=3, method="", filename=paste0(infolder, "HUC12_zones_10m_NAD83UTM.tif"))
zones.90m <- raster::aggregate(zones, fact=3, fun=modal, filename=paste0(infolder, "HUC12_zones_90m_NAD83UTM.tif"))
zones.270m <- raster::aggregrate(zones, fact=3, fun=modal, method="ngb", filename=paste0(infolder, "HUC12_zones_270m_NAD83UTM.tif"))


# Next, need to reproject encroachment condition layers to LEAFLET (delete old versions first)
infolder <- "C:/Users/Tyler/Desktop/New encroachment outputs/"   # folder holding original condition rasters
outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Condition Rasters 90m LEAFLET/"   # folder to hold reprojected condition rasters

mask <- stack(list.files(infolder, pattern=glob2rx("*tif$"), full.names=TRUE))  # stack condition rasters
mask.reproj <- projectRaster(mask, crs="+init=epsg:3857", method="ngb")  # reproject to Leaflet EPSG

mask.reproj.unstack <- unstack(mask.reproj)
outputnames <- paste(outfolder, names(mask.reproj), ".tif",sep="")
for(i in seq_along(mask.reproj.unstack)){writeRaster(mask.reproj.unstack[[i]], file=outputnames[i], overwrite=TRUE)}








# Create 270m zones layer and reproject to LEAFLET

zones.90m <- raster("C:/Work/SpatialData/NFWF_Cross_Realm/Final Input Rasters/HUC12_zones_90m_NAD83UTM.tif")  # read in 90m zones layer
zones.270m <- raster::aggregate(zones.90m, fact=3, fun=modal, method="ngb", filename="C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones/HUC12_zones_270m_NAD83UTM.tif", overwrite=TRUE)  # aggregate to 270m
zones.90m.leaflet <- projectRaster(zones.90m, res=90, crs="+init=epsg:3857", method="ngb", filename="C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones LEAFLET/HUC12_zones_90m_LEAFLET.tif")  # reproject
zones.270m.leaflet <- projectRaster(zones.270m, res=270, crs="+init=epsg:3857", method="ngb", filename="C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones LEAFLET/HUC12_zones_270m_LEAFLET.tif")  # reproject

writeRaster(zones.90m, file="C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones NAD83UTM/HUC12_zones_90m_NAD83UTM.tif", datatype="INT1U", overwrite=TRUE)
writeRaster(zones.270m, file="C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones NAD83UTM/HUC12_zones_270m_NAD83UTM.tif", datatype="INT1U", overwrite=TRUE)
writeRaster(zones.90m.leaflet, file="C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones LEAFLET/HUC12_zones_90m_LEAFLET.tif", datatype="INT1U", overwrite=TRUE)
writeRaster(zones.270m.leaflet, file="C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones LEAFLET/HUC12_zones_270m_LEAFLET.tif", datatype="INT1U", overwrite=TRUE)


writeRaster(zones.90m, file="C:/Users/Tyler/Desktop/zones90m.tif", datatype="INT1U", overwrite=TRUE)
writeRaster(zones.270m, file="C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones NAD83UTM/HUC12_zones_270m_NAD83UTM.tif", datatype="INT1U", overwrite=TRUE)
writeRaster(zones.90m.leaflet, file="C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones LEAFLET/HUC12_zones_90m_LEAFLET.tif", datatype="INT1U", overwrite=TRUE)
writeRaster(zones.270m.leaflet, file="C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Zones LEAFLET/HUC12_zones_270m_LEAFLET.tif", datatype="INT1U", overwrite=TRUE)






# Create 270m mask layers and reproject to LEAFLET while keeping alignment with zones layer

infolder <- "C:/Users/Tyler/Desktop/Final Masks 90m-20180503T213755Z-001/Final Masks 90m/"   # folder holding original condition rasters
#infolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Masks 90m/"
#outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Masks 270m LEAFLET/"   # folder to hold reprojected condition rasters
outfolder <- "C:/Users/Tyler/Desktop/Final Masks 270m LEAFLET/"

mask <- stack(list.files(infolder, pattern=glob2rx("*tif$"), full.names=TRUE))  # stack rasters 
reclass.mat <- matrix(c(0,1,1,NA), ncol=2, byrow=TRUE)  # reclassification matrix (old 1s become NA, old zeros become 1)
mask.reclass <- reclassify(mask, reclass.mat)   # reclassify such that areas where action applies=NA, areas where action doesn't apply=1 
mask.agg <- raster::aggregate(mask.reclass, fact=3, fun="modal")  # aggregate to 270m
mask.reproj <- projectRaster(mask.agg, zones.270m.leaflet, method="ngb")  # reproject to match projection, resolution, and alignment of 270m zones layer
mask.reproj.unstack <- unstack(mask.reproj)
outputnames <- paste(outfolder, substr(names(mask),1,nchar(names(mask))-12), "270m_LEAFLET.tif", sep="")
for(i in seq_along(mask.reproj.unstack)){writeRaster(mask.reproj.unstack[[i]], file=outputnames[i], datatype="INT1U", overwrite=TRUE)}




# Create 270m condition layers and reproject to LEAFLET while keeping alignment with zones layer

infolder <- "C:/Users/Tyler/Desktop/Final Condition Rasters 90m-20180503T230813Z-001/Final Condition Rasters 90m/"   # folder holding original condition rasters
#infolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Masks 90m/"
#outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Masks 270m LEAFLET/"   # folder to hold reprojected condition rasters
outfolder <- "C:/Users/Tyler/Desktop/Final Condition Rasters 270m LEAFLET/"

condition <- stack(list.files(infolder, pattern=glob2rx("*tif$"), full.names=TRUE))  # stack rasters 
condition.agg <- raster::aggregate(condition, fact=3, fun="modal")  # aggregate to 270m
condition.reproj <- projectRaster(condition.agg, zones.270m.leaflet, method="ngb")  # reproject to match projection, resolution, and alignment of 270m zones layer
condition.reproj.unstack <- unstack(condition.reproj)
outputnames <- paste(outfolder, substr(names(condition),1,nchar(names(condition))-8), "270m_LEAFLET.tif", sep="")
for(i in seq_along(condition.reproj.unstack)){writeRaster(condition.reproj.unstack[[i]], file=outputnames[i], overwrite=TRUE)}









### Code for reprojecting masks to 270-m NAD83 (not UTM) for better display in GUI


zones.90m <- raster("C:/Work/SpatialData/NFWF_Cross_Realm/Final Input Rasters/HUC12_zones_90m_NAD83UTM.tif")  # read in 90m zones layer
zones.270m <- raster::aggregate(zones.90m, fact=3, fun=modal, method="ngb")  # aggregate to 270m
zones.270m.nad83 <- projectRaster(zones.270m, crs="+init=epsg:4269", method="ngb")  # reproject to NAD83

# Create 270m mask layers and reproject to LEAFLET while keeping alignment with zones layer

infolder <- "C:/Users/Tyler/Desktop/Final Masks 90m-20180503T213755Z-001/Final Masks 90m/"   # folder holding original condition rasters
#infolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Masks 90m/"
#outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Final Masks 270m LEAFLET/"   # folder to hold reprojected condition rasters
outfolder <- "C:/Users/Tyler/Desktop/Final Mask 270m-ish NAD83/"

mask <- stack(list.files(infolder, pattern=glob2rx("*tif$"), full.names=TRUE))  # stack rasters 
reclass.mat <- matrix(c(0,1,1,NA), ncol=2, byrow=TRUE)  # reclassification matrix (old 1s become NA, old zeros become 1)
mask.reclass <- reclassify(mask, reclass.mat)   # reclassify such that areas where action applies=NA, areas where action doesn't apply=1 
mask.agg <- raster::aggregate(mask.reclass, fact=3, fun="modal")  # aggregate to 270m
mask.reproj <- projectRaster(mask.agg, zones.270m.nad83, method="ngb")  # reproject to match projection, resolution, and alignment of 270m zones layer
mask.reproj.unstack <- unstack(mask.reproj)
outputnames <- paste(outfolder, substr(names(mask),1,nchar(names(mask))-12), "270m_NAD83.tif", sep="")
for(i in seq_along(mask.reproj.unstack)){writeRaster(mask.reproj.unstack[[i]], file=outputnames[i], datatype="INT1U", overwrite=TRUE)}
