library(raster)
library(sf)

infolder <- "C:/Work/SpatialData/NFWF_Cross_Realm/Final Input Rasters/"  # folder holding prepped input rasters (should have same projection, resolution, and origin as HUC12 zones layer)
#outfolder <- "C:/Work/SpatialData/NFWF_Cross_Realm/Final Condition Rasters/"   # folder where condition raster will be written
outfolder <- "H:/Final Condition Rasters 90m/"
zones.10m <- raster(paste0(infolder, "HUC12_zones_10m_NAD83UTM.tif"))
zones.90m <- raster(paste0(infolder, "HUC12_zones_90m_NAD83UTM.tif"))




# Test for differences in extents of rasters
e1 <- extent(zones.10m)
e2 <- extent(keep.grazed.biotic)
plot(e1, col="blue")
plot(e2, col="red", add=TRUE)
compareRaster(zones.10m, keep.grazed.biotic)







########################################################################################################################
### FOREST/SHRUBLAND FUELS MANAGEMENT
########################################################################################################################

### Fuels mgmt --> riparian configuration
vb <- raster(paste0(infolder, "valley_bottom_no_waterbody_10m_NAD83UTM.tif"))  # import valley bottoms layer
fire <- raster(paste0(infolder, "wildfire_hazard_high_and_veryhigh_10m_NAD83UTM.tif"))   # import wildfire risk layer
highrisk.vb <- vb * fire   # keep only those pixels that are both high/very high fire risk and valley bottom
zones.10m.crop <- crop(zones.10m, highrisk.vb)  # crop by extent of smaller raster
zonal.mat <- zonal(x=highrisk.vb, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit (index of area)
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "fuelsMgmt_riparianConfig_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Fuels mgmt --> Upland veg
uplands <- raster(paste0(infolder, "uplands_10m_NAD83UTM.tif"))  # import uplands layer
fire <- raster(paste0(infolder, "wildfire_hazard_high_and_veryhigh_10m_NAD83UTM.tif"))   # import wildfire risk layer
highrisk.uplands <- uplands * fire   # keep only those pixels that are both high/very high fire risk and uplands
zones.10m.crop <- crop(zones.10m, highrisk.uplands)  # crop by extent of smaller raster
zonal.mat <- zonal(x=highrisk.uplands, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit (index of area)
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "fuelsMgmt_uplandVeg_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory



### Fuels mgmt --> Shrubland connectivity
shrub.centrality <- raster(paste0(infolder, "shrubland_centrality_flowlines_10m_NAD83UTM.tif"))  # import shrubland centrality layer
fire <- raster(paste0(infolder, "wildfire_hazard_high_and_veryhigh_10m_NAD83UTM.tif"))   # import wildfire risk layer
highrisk.centrality <- shrub.centrality * fire   # keep only those flowlines that are in areas of high wildfire risk
zones.10m.crop <- crop(zones.10m, highrisk.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=highrisk.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "fuelsMgmt_shrubCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Fuels mgmt --> Forest connectivity
forest.centrality <- raster(paste0(infolder, "forest_centrality_flowlines_10m_NAD83UTM.tif"))  # import forest centrality layer
fire <- raster(paste0(infolder, "wildfire_hazard_high_and_veryhigh_10m_NAD83UTM.tif"))   # import wildfire risk layer
highrisk.centrality <- forest.centrality * fire   # keep only those flowlines that are in areas of high wildfire risk
zones.10m.crop <- crop(zones.10m, highrisk.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=highrisk.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "fuelsMgmt_forestCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory



########################################################################################################################
### GRAZING MANAGEMENT
########################################################################################################################

### Grazing mgmt --> stream health
reversed.stream.biotic <- raster(paste0(infolder,"stream_biotic_condition_10m_NAD83UTM.tif"))  # import stream biotic condition layer, reversed so that poorer condition = higher value
graze <- raster(paste0(infolder, "public_grazing_lands_90m_buffer_10m_NAD83UTM.tif"))  # import buffered public grazing allotments layer
grazed.biotic <- reversed.stream.biotic * graze  # keep only those biotic condition pixels for areas that are grazed
meanval <- cellStats(grazed.biotic, stat="mean", na.rm=TRUE)  # calculate mean biotic value for streams on grazed lands
keep.grazed.biotic <- grazed.biotic
keep.grazed.biotic[keep.grazed.biotic<meanval] <- NA  # make a copy of stream biotic condition that will only include streams to restore (keepers)
zones.10m.crop <- crop(zones.10m, keep.grazed.biotic)  # crop by extent of smaller raster
zonal.mat <- zonal(x=keep.grazed.biotic, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "grazingMgmt_streamHealth_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory





##### DONE PROCESSING UP TO HERE





### Grazing mgmt --> riparian configuration
vb <- raster(paste0(infolder, "valley_bottom_no_waterbody_10m_NAD83UTM.tif"))  # import valley bottoms layer
graze <- raster(paste0(infolder, "public_grazing_lands_90m_buffer_NAD83UTM.tif"))  # import buffered public grazing allotments layer
grazed.vb <- vb * graze  # keep only those pixels that are both grazed and valley bottoms
zones.10m.crop <- crop(zones.10m, grazed.vb)  # crop by extent of smaller raster
zonal.mat <- zonal(x=grazed.vb, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit (index of area)
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "grazingMgmt_riparianConfig_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Grazing mgmt --> Upland veg
uplands <- raster(paste0(infolder, "uplands_10m_NAD83UTM.tif"))  # import uplands layer
graze <- raster(paste0(infolder, "public_grazing_lands_90m_buffer_10m_NAD83UTM.tif"))  # import buffered public grazing allotments layer
grazed.uplands <- uplands * graze   # keep only those pixels that are both grazed and uplands
zones.10m.crop <- crop(zones.10m, grazed.uplands)  # crop by extent of smaller raster
zonal.mat <- zonal(x=grazed.uplands, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit (index of area)
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "grazingMgmt_uplandVeg_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Grazing mgmt --> grassland connectivity
grass.centrality <- raster(paste0(infolder, "grassland_centrality_flowlines_10m_NAD83UTM.tif"))  # import grassland centrality layer
graze <- raster(paste0(infolder, "public_grazing_lands_90m_buffer_10m_NAD83UTM.tif"))  # import buffered public grazing allotments layer
graze.centrality <- grass.centrality * graze   # keep only those flowlines that are in public grazing allotments
zones.10m.crop <- crop(zones.10m, graze.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=graze.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "grazingMgmt_grassCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Grazing mgmt --> shrubland connectivity
shrub.centrality <- raster(paste0(infolder, "shrubland_centrality_flowlines_10m_NAD83UTM.tif"))  # import shrubland centrality layer
graze <- raster(paste0(infolder, "public_grazing_lands_90m_buffer_10m_NAD83UTM.tif"))  # import buffered public grazing allotments layer
graze.centrality <- shrub.centrality * graze   # keep only those flowlines that are in public grazing allotments
zones.10m.crop <- crop(zones.10m, graze.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=graze.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "grazingMgmt_shrubCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Grazing mgmt --> forest connectivity
forest.centrality <- raster(paste0(infolder, "forest_centrality_flowlines_10m_NAD83UTM.tif"))  # import forest centrality layer
graze <- raster(paste0(infolder, "public_grazing_lands_90m_buffer_10m_NAD83UTM.tif"))  # import buffered public grazing allotments layer
graze.centrality <- forest.centrality * graze   # keep only those flowlines that are in public grazing allotments
zones.10m.crop <- crop(zones.10m, graze.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=graze.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "grazingMgmt_forestCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory






########################################################################################################################
### IRRIGATION ADJUSTMENT
########################################################################################################################

### Irrigation adjustment --> Normative flow regime
  # first make a reversed CFM shift layer (like I did for stream Health) - write to hard drive, then import here

### Irrigation adjustment --> Stream health
reversed.stream.biotic <- raster(paste0(infolder,"stream_biotic_condition_10m_NAD83UTM.tif"))  # import stream biotic condition layer, reversed so that poorer condition = higher value
irrig <- raster(paste0(infolder, "irrigated_lands_90m_buffer_10m_NAD83UTM.tif"))  # import buffered irrigated lands layer
irrig.biotic <- reversed.stream.biotic * irrig  # keep only those biotic condition pixels for areas that are irrigated
meanval <- cellStats(irrig.biotic, stat="mean", na.rm=TRUE)  # calculate mean biotic value for streams on irrigated lands
keep.irrig.biotic <- irrig.biotic
keep.irrig.biotic[keep.irrig.biotic<meanval] <- NA  # make a copy of stream biotic condition that will only include streams to restore (keepers)
zones.10m.crop <- crop(zones.10m, keep.irrig.biotic)  # crop by extent of smaller raster
zonal.mat <- zonal(x=keep.irrig.biotic, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "irrigAdjust_streamHealth_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory






########################################################################################################################
### Land protection
########################################################################################################################

### Land protection --> Normative flow regime


### Land protection --> Riparian configuration
vb <- raster(paste0(infolder, "valley_bottom_no_waterbody_10m_NAD83UTM.tif"))  # import valley bottoms layer
privatecons <- raster(paste0(infolder, "conservation_easement_mask_10m_NAD83UTM.tif"))  # import private, non-conserved lands layer
hmi <- raster(paste0(infolder, "HMI_10m_NAD83UTM.tif"))  # import human modification index layer
privatecons.vb <- vb * privatecons   # keep only those pixels that are both private, non-conserved lands and valley bottoms
privatecons.vb.hmi <- privatecons.vb * hmi   # HMI for private, non-conserved valley bottoms
meanval <- cellStats(privatecons.vb.hmi, stat="mean", na.rm=TRUE)  # calculate mean HMI for private, non-conserved valley bottoms
keep.privatecons.vb <- privatecons.vb.hmi
keep.privatecons.vb[keep.privatecons.vb<meanval] <- 1  # keep pixels with HMI less than mean (i.e., best candidates for conservation)
keep.privatecons.vb[keep.privatecons.vb>meanval] <- NA  
zones.10m.crop1 <- crop(zones.10m, keep.privatecons.vb)  # crop by extent of smaller raster
zonal.mat1 <- zonal(x=keep.privatecons.vb, z=zones.10m.crop1, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit (index of area)
zonal.mat1[zonal.mat1==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original1 <- zonal.mat1[,2] # vector of raw condition values
rescaled1 <- (original1 - min(original1, na.rm=TRUE))/(max(original1, na.rm=TRUE)-min(original1, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
sinuosity <- raster(paste0(infolder, "stream_sinuosity_10m_NAD83UTM.tif"))  # import sinuosity layer
privatecons.sinuosity <- keep.privatecons.vb * sinuosity  # sinuosity of streams within minimally modified, private, non-conserved valley bottoms only
zones.10m.crop2 <- crop(zones.10m, privatecons.sinuosity)  # crop by extent of smaller raster
zonal.mat2 <- zonal(x=privatecons.sinuosity, z=zones.10m.crop2, fun="mean", na.rm=TRUE)   # take mean of remaining pixels
zonal.mat2[zonal.mat2==0 | zonal.mat2=="NaN"] <- NA   # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original2 <- zonal.mat2[,2] # vector of raw condition values
rescaled2 <- (original2 - min(original2, na.rm=TRUE))/(max(original2, na.rm=TRUE)-min(original2, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
combo <- rescaled1 + rescaled2
rescaled.combo <- (combo - min(combo, na.rm=TRUE))/(max(combo, na.rm=TRUE)-min(combo, na.rm=TRUE))
reclass.mat <- as.matrix(cbind(zonal.mat1[,1],rescaled.combo))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landProtect_riparianConfig_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Land protection --> Stream health
stream.biotic <- raster(paste0(infolder,"stream_biotic_condition_10m_NAD83UTM.tif"))  # import stream biotic condition layer
privatecons <- raster(paste0(infolder, "conservation_easement_mask_10m_NAD83UTM.tif"))  # import private, non-conserved lands layer
privatecons.biotic <- stream.biotic * privatecons  # keep only those biotic condition pixels for areas within private, non-conserved lands
meanval <- cellStats(privatecons.biotic, stat="mean", na.rm=TRUE)  # calculate mean biotic value for within private, non-conserved lands
keep.privatecons.biotic <- privatecons.biotic
keep.privatecons.biotic[keep.privatecons.biotic<meanval] <- NA  # make a copy of stream biotic condition that will only include streams to conserve (keepers)
zones.10m.crop <- crop(zones.10m, keep.privatecons.biotic)  # crop by extent of smaller raster
zonal.mat <- zonal(x=keep.privatecons.biotic, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landProtect_streamHealth_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Land protection --> Upland veg
uplands <- raster(paste0(infolder, "uplands_10m_NAD83UTM.tif"))  # import uplands layer
privatecons <- raster(paste0(infolder, "conservation_easement_mask_10m_NAD83UTM.tif"))  # import private, non-conserved lands layer
privatecons.uplands <- uplands * privatecons   # keep only those pixels that are both private, non-conserved and uplands
hmi <- raster(paste0(infolder, "HMI_10m_NAD83UTM.tif"))  # import human modification index layer
privatecons.uplands.hmi <- privatecons.uplands * hmi   # HMI for private, non-conserved uplands
meanval <- cellStats(privatecons.uplands.hmi, stat="mean", na.rm=TRUE)  # calculate mean HMI for private, non-conserved uplands
keep.privatecons.uplands <- privatecons.uplands.hmi
keep.privatecons.uplands[keep.privatecons.uplands<meanval] <- 1  # keep pixels with HMI less than mean (i.e., best candidates for conservation)
keep.privatecons.uplands[keep.privatecons.uplands>meanval] <- NA  
zones.10m.crop <- crop(zones.10m, keep.privatecons.uplands)  # crop by extent of smaller raster
zonal.mat <- zonal(x=keep.privatecons.uplands, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit (index of area)
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landProtect_uplandVeg_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Land protection --> Grassland connectivity
grass.centrality <- raster(paste0(infolder, "grassland_centrality_flowlines_10m_NAD83UTM.tif"))  # import grassland centrality layer
privatecons <- raster(paste0(infolder, "conservation_easement_mask_10m_NAD83UTM.tif"))  # import private, non-conserved lands layer
privatecons.centrality <- grass.centrality * privatecons   # keep only those flowlines that are on private, non-conserved lands
zones.10m.crop <- crop(zones.10m, privatecons.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=privatecons.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landProtect_grassCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Land protection --> Shrubland connectivity
shrub.centrality <- raster(paste0(infolder, "shrubland_centrality_flowlines_10m_NAD83UTM.tif"))  # import shrubland centrality layer
privatecons <- raster(paste0(infolder, "conservation_easement_mask_10m_NAD83UTM.tif"))  # import private, non-conserved lands layer
privatecons.centrality <- shrub.centrality * privatecons   # keep only those flowlines that are on private, non-conserved lands
zones.10m.crop <- crop(zones.10m, privatecons.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=privatecons.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landProtect_shrubCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Land protection --> Forest connectivity
forest.centrality <- raster(paste0(infolder, "forest_centrality_flowlines_10m_NAD83UTM.tif"))  # import forest centrality layer
privatecons <- raster(paste0(infolder, "conservation_easement_mask_10m_NAD83UTM.tif"))  # import private, non-conserved lands layer
privatecons.centrality <- forest.centrality * privatecons   # keep only those flowlines that are on private, non-conserved lands
zones.10m.crop <- crop(zones.10m, privatecons.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=privatecons.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landProtect_forestCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory






########################################################################################################################
### Bridge/Culvert Upgrade
########################################################################################################################

### Bridge/culvert upgrade --> Stream health

### Bridge/culvert upgrade --> Within-habitat connectivity





########################################################################################################################
### Road Decommissioning
########################################################################################################################

### Road decommissioning --> Stream health

### Road decommissioning --> Grassland connectivity

### Road decommissioning --> Shrubland connectivity

### Road decommissioning --> Forest connectivity






########################################################################################################################
### Soil Health Management
########################################################################################################################

### Soil health management --> Normative flow regime

### Soil health management --> Stream health




########################################################################################################################
### Stream/Riparian Restoration
#########################################################################################################################

### Stream/ripa
















