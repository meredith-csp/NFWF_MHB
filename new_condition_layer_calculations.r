library(raster)
library(sf)

infolder <- "C:/Work/SpatialData/NFWF_Cross_Realm/Final Input Rasters/"  # folder holding prepped input rasters (should have same projection, resolution, and origin as HUC12 zones layer)
#outfolder <- "C:/Work/SpatialData/NFWF_Cross_Realm/Final Condition Rasters/"   # folder where condition raster will be written
outfolder <- "H:/Final Condition Rasters 90m/"
zones.10m <- raster(paste0(infolder, "HUC12_zones_10m_NAD83UTM.tif"))
zones.90m <- raster(paste0(infolder, "HUC12_zones_90m_NAD83UTM.tif"))




# Test for differences in extents of rasters
e1 <- extent(zones.10m)
e2 <- extent(keep.ag.biotic)
plot(e1, col="blue")
plot(e2, col="red", add=TRUE)
compareRaster(zones.10m, roads.vb)







########################################################################################################################
### FOREST/SHRUBLAND FUELS MANAGEMENT
########################################################################################################################


### Fuels mgmt --> riparian configuration
# Goal: Select HUC units that contain the most valley bottom with high wildfire risk
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
# Goal: Select HUC units that contain the most valley bottom with high wildfire risk
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
# Goal: Select HUC units that contain the most high centrality shrubland flowlines in areas with high wildfire risk
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
# Goal: Select HUC units that contain the most high centrality forest flowlines in areas with high wildfire risk
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
# Goal: Select HUC units that have the greatest length of streams in poor biotic condition on grazed lands
reversed.stream.biotic <- raster(paste0(infolder,"reversed_stream_biotic_condition_10m_NAD83UTM.tif"))  # import stream biotic condition layer, reversed so that poorer condition = higher value
graze <- raster(paste0(infolder, "public_grazing_lands_90m_buffer_10m_NAD83UTM.tif"))  # import buffered public grazing allotments layer
grazed.biotic <- reversed.stream.biotic * graze  # keep only those biotic condition pixels for areas that are grazed
meanval <- cellStats(grazed.biotic, stat="mean", na.rm=TRUE)  # calculate mean biotic value for streams on grazed lands
keep.grazed.biotic <- reclassify(grazed.biotic, rcl=matrix(c(0, meanval, NA), ncol=3), right=FALSE)  # keep only those streams with poorer than average biotic condition (good restoration candidates)
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


### Grazing mgmt --> riparian configuration
#Goal: Select HUC units with the greatest proportion of grazed valley bottoms
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
# Goal: Select HUC units with the greatest proportion of grazed uplands
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
# Goal: Select HUC units with the most high centrality grassland flowlines on grazed lands
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
# Goal: Select HUC units with the most high centrality shrubland flowlines on grazed lands
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
# Goal: Select HUC units with the most high centrality forest flowlines on grazed lands
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
# Goal: Select HUC units that have the greatest length of streams estimated to experience large shift in center of flow mass toward early-season flow on irrigated lands
cfm.early <- raster(paste0(infolder,"stream_CFM_days_early_10m_NAD83UTM.tif"))  # import CFM shift layer (# days earlier by 2040)
irrig <- raster(paste0(infolder, "irrigated_lands_90m_buffer_10m_NAD83UTM.tif"))  # import buffered irrigated lands layer
irrig.cfm <- irrig * cfm.early   # keep only those cfm pixels for areas that are irrigated
meanval <- cellStats(irrig.cfm, stat="mean", na.rm=TRUE)  # calculate mean CFM shift for streams on irrigated lands
keep.irrig.cfm <- reclassify(irrig.cfm, rcl=matrix(c(0, meanval, NA), ncol=3), right=FALSE)  # keep only those streams with higher than average early shift (good restoration candidates)
zones.10m.crop <- crop(zones.10m, keep.irrig.cfm)  # crop by extent of smaller raster
zonal.mat <- zonal(x=keep.irrig.cfm, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "irrigAdjust_normFlow_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Irrigation adjustment --> Stream health
# Goal: Select HUC units that have the greatest length of streams in poor biotic condition on irrigated lands
reversed.stream.biotic <- raster(paste0(infolder,"reversed_stream_biotic_condition_10m_NAD83UTM.tif"))  # import stream biotic condition layer, reversed so that poorer condition = higher value
irrig <- raster(paste0(infolder, "irrigated_lands_90m_buffer_10m_NAD83UTM.tif"))  # import buffered irrigated lands layer
irrig.biotic <- reversed.stream.biotic * irrig  # keep only those biotic condition pixels for areas that are irrigated
meanval <- cellStats(irrig.biotic, stat="mean", na.rm=TRUE)  # calculate mean biotic value for streams on irrigated lands
keep.irrig.biotic <- reclassify(irrig.biotic, rcl=matrix(c(0, meanval, NA), ncol=3), right=FALSE)  # keep only those streams with poorer than average biotic condition (good restoration candidates)
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
# Goal: Select HUC units that have the greatest length of streams estimated to experience minimal shift in center of flow mass toward early-season flow within private, non-conserved lands
reversed.cfm.early <- raster(paste0(infolder,"reversed_stream_CFM_days_early_10m_NAD83UTM.tif"))  # import reversed CFM shift layer (higher value=smaller early shift)
privatecons <- raster(paste0(infolder, "conservation_easement_mask_10m_NAD83UTM.tif"))  # import private, non-conserved lands layer
privatecons.cfm <- privatecons * reversed.cfm.early   # keep only those cfm pixels within private, non-conserved lands
meanval <- cellStats(privatecons.cfm, stat="mean", na.rm=TRUE)  # calculate mean CFM shift for streams on private, non-conserved lands
keep.privatecons.cfm <- reclassify(privatecons.cfm, rcl=matrix(c(0, meanval, NA), ncol=3), right=FALSE)  # keep only those streams with higher than average early shift (good restoration candidates)
zones.10m.crop <- crop(zones.10m, keep.privatecons.cfm)  # crop by extent of smaller raster
zonal.mat <- zonal(x=keep.privatecons.cfm, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landProtect_normFlow_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Land protection --> Riparian configuration
# Goal: Select HUC units that have (1) largest proportion of minimally modified valley bottom on private non-conserved lands, and (2) high stream sinuosity within these valley bottoms
vb <- raster(paste0(infolder, "valley_bottom_no_waterbody_10m_NAD83UTM.tif"))  # import valley bottoms layer
privatecons <- raster(paste0(infolder, "conservation_easement_mask_10m_NAD83UTM.tif"))  # import private, non-conserved lands layer
hmi <- raster(paste0(infolder, "HMI_10m_NAD83UTM.tif"))  # import human modification index layer
privatecons.vb <- vb * privatecons   # keep only those pixels that are both private, non-conserved lands and valley bottoms
privatecons.vb.hmi <- privatecons.vb * hmi   # HMI for private, non-conserved valley bottoms
meanval <- cellStats(privatecons.vb.hmi, stat="mean", na.rm=TRUE)  # calculate mean HMI for private, non-conserved valley bottoms
keep.privatecons.vb <- reclassify(privatecons.vb.hmi, rcl=matrix(c(0, meanval, 1, meanval, 1, NA), ncol=3, byrow=TRUE), right=FALSE)  # keep only those valley bottoms with lower than average HMI (good conservation candidates)
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
# Goal: Select HUC units that have the greatest length of streams in good biotic condition within private, non-conserved lands
stream.biotic <- raster(paste0(infolder,"stream_biotic_condition_10m_NAD83UTM.tif"))  # import stream biotic condition layer
privatecons <- raster(paste0(infolder, "conservation_easement_mask_10m_NAD83UTM.tif"))  # import private, non-conserved lands layer
privatecons.biotic <- stream.biotic * privatecons  # keep only those biotic condition pixels for areas within private, non-conserved lands
meanval <- cellStats(privatecons.biotic, stat="mean", na.rm=TRUE)  # calculate mean biotic value for within private, non-conserved lands
keep.privatecons.biotic <- reclassify(privatecons.biotic, rcl=matrix(c(0, meanval, NA), ncol=3), right=FALSE)  # keep only those streams with better than average biotic condition (good conservation candidates)
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
# Goal: Select HUC units that have the largest proportion of minimally modified uplands on private, non-conserved lands
uplands <- raster(paste0(infolder, "uplands_10m_NAD83UTM.tif"))  # import uplands layer
privatecons <- raster(paste0(infolder, "conservation_easement_mask_10m_NAD83UTM.tif"))  # import private, non-conserved lands layer
privatecons.uplands <- uplands * privatecons   # keep only those pixels that are both private, non-conserved and uplands
hmi <- raster(paste0(infolder, "HMI_10m_NAD83UTM.tif"))  # import human modification index layer
privatecons.uplands.hmi <- privatecons.uplands * hmi   # HMI for private, non-conserved uplands
meanval <- cellStats(privatecons.uplands.hmi, stat="mean", na.rm=TRUE)  # calculate mean HMI for private, non-conserved uplands
keep.privatecons.uplands <- reclassify(privatecons.uplands.hmi, rcl=matrix(c(0, meanval, 1, meanval, 1, NA), ncol=3, byrow=TRUE), right=FALSE)  # keep only those uplands with lower than average HMI (good conservation candidates)
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
# Goal: Select HUC units that have contain the most high centrality grassland flowlines within private, non-conserved lands
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
# Goal: Select HUC units that have contain the most high centrality shrubland flowlines within private, non-conserved lands
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
# Goal: Select HUC units that have contain the most high centrality forest flowlines within private, non-conserved lands
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
# Goal: Select HUC units with bridges near poor condition streams
reversed.stream.biotic <- raster(paste0(infolder,"reversed_stream_biotic_condition_10m_NAD83UTM.tif"))  # import stream biotic condition layer, reversed so that poorer condition = higher value
bridge.buffer <- raster(paste0(infolder, "bridges_100m_buffer_10m_NAD83UTM.tif"))  # import bridges layer, buffered by 100 m
bridge.biotic <- bridge.buffer * reversed.stream.biotic  # keep only those biotic values for stream segments within 100m of bridges
zones.10m.crop <- crop(zones.10m, bridge.biotic)  # crop by extent of smaller raster
zonal.mat <- zonal(x=bridge.biotic, z=zones.10m.crop, fun="mean", na.rm=TRUE)   # take mean of remaining pixels
zonal.mat[zonal.mat==0 | zonal.mat=="NaN"] <- NA   # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "bridgeCulvert_streamHealth_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Bridge/culvert upgrade --> forest connectivity
# Goal: Select HUC units where the most high centrality forest flowlines are present in the vicinity of bridges/culverts that could potentially be upgraded
forest.centrality <- raster(paste0(infolder, "forest_centrality_flowlines_10m_NAD83UTM.tif"))  # import forest centrality layer
bridge.buffer <- raster(paste0(infolder, "bridges_1km_buffer_10m_NAD83UTM.tif"))  # import bridges layer, buffered by 1 km
bridge.centrality <- forest.centrality * bridge.buffer   # keep only those flowlines that are within 1 km of bridges
zones.10m.crop <- crop(zones.10m, bridge.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=bridge.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "bridgeCulvert_forestCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Bridge/culvert upgrade --> grassland connectivity
# Goal: Select HUC units where the most high centrality grassland flowlines are present in the vicinity of bridges/culverts that could potentially be upgraded
grass.centrality <- raster(paste0(infolder, "grassland_centrality_flowlines_10m_NAD83UTM.tif"))  # import grassland centrality layer
bridge.buffer <- raster(paste0(infolder, "bridges_1km_buffer_10m_NAD83UTM.tif"))  # import bridges layer, buffered by 1 km
bridge.centrality <- grass.centrality * bridge.buffer   # keep only those flowlines that are within 1 km of bridges
zones.10m.crop <- crop(zones.10m, bridge.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=bridge.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "bridgeCulvert_grassCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Bridge/culvert upgrade --> shrubland connectivity
# Goal: Select HUC units where the most high centrality shrubland flowlines are present in the vicinity of bridges/culverts that could potentially be upgraded
shrub.centrality <- raster(paste0(infolder, "shrubland_centrality_flowlines_10m_NAD83UTM.tif"))  # import shrubland centrality layer
bridge.buffer <- raster(paste0(infolder, "bridges_1km_buffer_10m_NAD83UTM.tif"))  # import bridges layer, buffered by 1 km
bridge.centrality <- shrub.centrality * bridge.buffer   # keep only those flowlines that are within 1 km of bridges
zones.10m.crop <- crop(zones.10m, bridge.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=bridge.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "bridgeCulvert_shrubCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory





########################################################################################################################
### Road Decommissioning
########################################################################################################################

### Road decommissioning --> Riparian configuration
# Goal: Select HUC units that have the greatest amount of decommissionable roads in valley bottoms
roads <- raster(paste0(infolder, "decommissionable_roads_10m_NAD83UTM.tif"))    # import decommissionable roads layer
vb <- raster(paste0(infolder, "valley_bottom_no_waterbody_10m_NAD83UTM.tif"))  # import valley bottoms layer
roads.vb <- roads * vb   # remove roads that are not within valleybottom
zones.10m.crop <- crop(zones.10m, roads.vb)  # crop by extent of smaller raster
zonal.mat <- zonal(x=roads.vb, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "roadDecommission_riparianConfig_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Road decommissioning --> Stream health
# Goal: Select HUC units that have the greatest length of streams in poor biotic condition near decommissionable roads
reversed.stream.biotic <- raster(paste0(infolder,"reversed_stream_biotic_condition_10m_NAD83UTM.tif"))  # import stream biotic condition layer, reversed so that poorer condition = higher value
road.buffer <- raster(paste0(infolder, "decommissionable_roads_1km_buffer_10m_NAD83UTM.tif"))  # import roads buffered by 1 km layer
road.biotic <- reversed.stream.biotic * road.buffer  # keep only those biotic condition pixels for areas near roads
meanval <- cellStats(road.biotic, stat="mean", na.rm=TRUE)  # calculate mean biotic value for streams near roads
keep.road.biotic <- reclassify(road.biotic, rcl=matrix(c(0, meanval, NA), ncol=3), right=FALSE)
keep.road.biotic.crop <- crop(keep.road.biotic, zones.10m)  # crop by extent of smaller raster
zonal.mat <- zonal(x=keep.road.biotic.crop, z=zones.10m, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "roadDecommission_streamHealth_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Road decommissioning --> Grassland connectivity
# Goal: Select HUC units that have contain the most high centrality grassland flowlines near decommissionable roads
grass.centrality <- raster(paste0(infolder, "grassland_centrality_flowlines_10m_NAD83UTM.tif"))  # import grassland centrality layer
road.buffer <- raster(paste0(infolder, "decommissionable_roads_1km_buffer_10m_NAD83UTM.tif"))  # import roads buffered by 1 km layer
road.centrality <- grass.centrality * road.buffer   # keep only those flowlines that are within 1 km of roads
zones.10m.crop <- crop(zones.10m, road.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=road.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "roadDecommission_grassCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Road decommissioning --> Shrubland connectivity
# Goal: Select HUC units that have contain the most high centrality shrubland flowlines near decommissionable roads
shrub.centrality <- raster(paste0(infolder, "shrubland_centrality_flowlines_10m_NAD83UTM.tif"))  # import shrubland centrality layer
road.buffer <- raster(paste0(infolder, "decommissionable_roads_1km_buffer_10m_NAD83UTM.tif"))  # import roads buffered by 1 km layer
road.centrality <- shrub.centrality * road.buffer   # keep only those flowlines that are within 1 km of roads
zones.10m.crop <- crop(zones.10m, road.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=road.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "roadDecommission_shrubCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Road decommissioning --> Forest connectivity
# Goal: Select HUC units that have contain the most high centrality forest flowlines near decommissionable roads
forest.centrality <- raster(paste0(infolder, "forest_centrality_flowlines_10m_NAD83UTM.tif"))  # import forest centrality layer
road.buffer <- raster(paste0(infolder, "decommissionable_roads_1km_buffer_10m_NAD83UTM.tif"))  # import roads buffered by 1 km layer
road.centrality <- forest.centrality * road.buffer   # keep only those flowlines that are within 1 km of roads
zones.10m.crop <- crop(zones.10m, road.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=road.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "roadDecommission_forestCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory




########################################################################################################################
### Soil Health Management
########################################################################################################################

### Soil health management --> Normative flow regime
# Goal: Select HUC units that have the greatest length of streams estimated to experience large shift in center of flow mass toward early-season flow on agricultural lands
cfm.early <- raster(paste0(infolder,"stream_CFM_days_early_10m_NAD83UTM.tif"))  # import CFM shift layer (# days earlier by 2040)
ag <- raster(paste0(infolder, "agricultural_land_90m_buffer_10m_NAD83UTM.tif"))  # import agricultural lands layer (includes row crops and pasture/hay)
ag.cfm <- ag * cfm.early   # keep only those cfm pixels for areas that are cultivated
meanval <- cellStats(ag.cfm, stat="mean", na.rm=TRUE)  # calculate mean CFM shift for streams on cultivated lands
keep.ag.cfm <- reclassify(ag.cfm, rcl=matrix(c(0, meanval, NA), ncol=3), right=FALSE)   # keep only those streams with higher than average early shift (good candidates for restoration)
zones.10m.crop <- crop(zones.10m, keep.ag.cfm)  # crop by extent of smaller raster
zonal.mat <- zonal(x=keep.ag.cfm, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "soilMgmt_normFlow_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Soil health management --> Stream health
# Goal: Select HUC units that have the greatest length of streams in poor biotic condition on agricultural lands
reversed.stream.biotic <- raster(paste0(infolder,"reversed_stream_biotic_condition_10m_NAD83UTM.tif"))  # import stream biotic condition layer, reversed so that poorer condition = higher value
ag <- raster(paste0(infolder, "agricultural_land_90m_buffer_10m_NAD83UTM.tif"))  # import agricultural lands layer (includes row crops and pasture/hay)
ag.biotic <- reversed.stream.biotic * ag  # keep only those biotic condition pixels for areas that are cultivated
meanval <- cellStats(ag.biotic, stat="mean", na.rm=TRUE)  # calculate mean biotic value for streams on cultivated lands
keep.ag.biotic <- reclassify(ag.biotic, rcl=matrix(c(0, meanval, NA), ncol=3), right=FALSE)  # keep only those streams with poorer than average biotic condition (good candidates for restoration)
zones.10m.crop <- crop(zones.10m, keep.ag.biotic)  # crop by extent of smaller raster
zonal.mat <- zonal(x=keep.ag.biotic, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "soilMgmt_streamHealth_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory




########################################################################################################################
### Stream/Riparian Restoration
#########################################################################################################################

### Stream/riparian restoration --> normative flow regime
# Goal: Select HUC units that have the greatest length of streams estimated to experience large shift in center of flow mass toward early-season flow
cfm.early <- raster(paste0(infolder,"stream_CFM_days_early_10m_NAD83UTM.tif"))  # import CFM shift layer (# days earlier by 2040)
meanval <- cellStats(cfm.early, stat="mean", na.rm=TRUE)  # calculate mean CFM shift for streams
keep.cfm <- reclassify(cfm.early, rcl=matrix(c(0, meanval, NA), ncol=3), right=FALSE)   # keep only those streams with higher than average early shift (good candidates for restoration)
zones.10m.crop <- crop(zones.10m, keep.cfm)  # crop by extent of smaller raster
zonal.mat <- zonal(x=keep.cfm, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "streamRipRestore_normFlow_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Stream/riparian restoration --> riparian configuration
# Goal: Select HUC units that have (1) the largest proportion of valley bottom that is heavily modified, and (2) low stream sinuosity within these valley bottoms
vb <- raster(paste0(infolder, "valley_bottom_no_waterbody_10m_NAD83UTM.tif"))  # import valley bottoms layer
hmi <- raster(paste0(infolder, "HMI_10m_NAD83UTM.tif"))  # import human modification index layer
nondev <- raster(paste0(infolder, "nondeveloped_cover_types_10m_NAD83UTM.tif"))  # import nondeveloped cover types layer 
nondev.vb <- vb * nondev   # keep only those pixels that are both nondeveloped and valley bottoms
nondev.vb.hmi <- nondev.vb * hmi   # HMI for nondeveloped valley bottoms
meanval <- cellStats(nondev.vb.hmi, stat="mean", na.rm=TRUE)  # calculate mean HMI for nondeveloped valley bottoms
keep.nondev.vb <- reclassify(nondev.vb.hmi, rcl=matrix(c(0, meanval, NA, meanval, 1, 1), ncol=3, byrow=TRUE), right=FALSE)  # keep only those valley bottoms with lower than average HMI (good conservation candidates)
zones.10m.crop1 <- crop(zones.10m, keep.nondev.vb)  # crop by extent of smaller raster
zonal.mat1 <- zonal(x=keep.nondev.vb, z=zones.10m.crop1, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit (index of area)
zonal.mat1[zonal.mat1==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original1 <- zonal.mat1[,2] # vector of raw condition values
rescaled1 <- (original1 - min(original1, na.rm=TRUE))/(max(original1, na.rm=TRUE)-min(original1, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
sinuosity <- raster(paste0(infolder, "stream_sinuosity_10m_NAD83UTM.tif"))  # import sinuosity layer
nondev.sinuosity <- keep.nondev.vb * sinuosity  # sinuosity of streams within minimally modified, private, non-conserved valley bottoms only
zones.10m.crop2 <- crop(zones.10m, nondev.sinuosity)  # crop by extent of smaller raster
zonal.mat2 <- zonal(x=nondev.sinuosity, z=zones.10m.crop2, fun="mean", na.rm=TRUE)   # take mean of remaining pixels
zonal.mat2[zonal.mat2==0 | zonal.mat2=="NaN"] <- NA   # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original2 <- zonal.mat2[,2] # vector of raw condition values
rescaled2 <- 1 - (original2 - min(original2, na.rm=TRUE))/(max(original2, na.rm=TRUE)-min(original2, na.rm=TRUE))  # NEGATIVE RELATIONSHIP rescale condition values from 0-1
combo <- rescaled1 + rescaled2
rescaled.combo <- (combo - min(combo, na.rm=TRUE))/(max(combo, na.rm=TRUE)-min(combo, na.rm=TRUE))
reclass.mat <- as.matrix(cbind(zonal.mat1[,1],rescaled.combo))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "streamRipRestore_riparianConfig_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Stream/riparian restoration --> stream health
# Goal: Select HUC units that have the greatest length of streams in poor biotic condition
reversed.stream.biotic <- raster(paste0(infolder,"reversed_stream_biotic_condition_10m_NAD83UTM.tif"))  # import stream biotic condition layer, reversed so that poorer condition = higher value
meanval <- cellStats(reversed.stream.biotic, stat="mean", na.rm=TRUE)  # calculate mean biotic value for streams
keep.biotic <- reclassify(reversed.stream.biotic, rcl=matrix(c(0, meanval, NA), ncol=3), right=FALSE)  # keep those streams with poorer than average biotic condition (good restoration candidates)
zones.10m.crop <- crop(zones.10m, keep.biotic)  # crop by extent of smaller raster
zonal.mat <- zonal(x=keep.biotic, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "streamRipRestore_streamHealth_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Stream/riparian restoration --> grassland connectivity
# Goal: Select HUC units that have contain the most high centrality grassland flowlines within valley bottoms
grass.centrality <- raster(paste0(infolder, "grassland_centrality_flowlines_10m_NAD83UTM.tif"))  # import grassland centrality layer
vb <- raster(paste0(infolder, "valley_bottom_no_waterbody_10m_NAD83UTM.tif"))  # import valley bottoms layer
vb.centrality <- grass.centrality * vb   # keep only those flowlines that are within valley bottoms
zones.10m.crop <- crop(zones.10m, vb.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=vb.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "streamRipRestore_grassCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Stream/riparian restoration --> forest connectivity
# Goal: Select HUC units that have contain the most high centrality forest flowlines within valley bottoms
forest.centrality <- raster(paste0(infolder, "forest_centrality_flowlines_10m_NAD83UTM.tif"))  # import forest centrality layer
vb <- raster(paste0(infolder, "valley_bottom_no_waterbody_10m_NAD83UTM.tif"))  # import valley bottoms layer
vb.centrality <- forest.centrality * vb   # keep only those flowlines that are within valley bottoms
zones.10m.crop <- crop(zones.10m, vb.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=vb.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "streamRipRestore_forestCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Stream/riparian restoration --> shrubland connectivity
# Goal: Select HUC units that have contain the most high centrality shrubland flowlines within valley bottoms
shrub.centrality <- raster(paste0(infolder, "shrubland_centrality_flowlines_10m_NAD83UTM.tif"))  # import shrubland centrality layer
vb <- raster(paste0(infolder, "valley_bottom_no_waterbody_10m_NAD83UTM.tif"))  # import valley bottoms layer
vb.centrality <- shrub.centrality * vb   # keep only those flowlines that are within valley bottoms
zones.10m.crop <- crop(zones.10m, vb.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=vb.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "streamRipRestore_shrubCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory




########################################################################################################################
### Conifer/Shrub Encroachment Control
#########################################################################################################################

### Conifer/shrub control --> Upland veg
# Goal: Select HUC units with the greatest area of upland area near an edge between grassland and forest or shrub 
uplands <- raster(paste0(infolder, "uplands_10m_NAD83UTM.tif"))  # import uplands layer
edge <- raster(paste0(infolder, "               "))
edge.uplands <- uplands * edge   # keep only those pixels that are both  and grass/forest/shrub edge
zones.10m.crop <- crop(zones.10m, edge.uplands)  # crop by extent of smaller raster
zonal.mat <- zonal(x=edge.uplands, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit (index of area)
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "encroachControl_uplandVeg_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Conifer/shrub control --> grassland connectivity
# Goal: Select HUC units that contain the high centrality grassland flowlines near an edge between grassland and forest or shrub
grass.centrality <- raster(paste0(infolder, "grassland_centrality_flowlines_10m_NAD83UTM.tif"))  # import grassland centrality layer
edge <- raster(paste0(infolder, "               "))
edge.centrality <- grass.centrality * edge   # keep only those flowlines that are within valley bottoms
zones.10m.crop <- crop(zones.10m, edge.centrality)  # crop by extent of smaller raster
zonal.mat <- zonal(x=edge.centrality, z=zones.10m.crop, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones.90m, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "encroachControl_grassCentrality_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "zones.90m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory






