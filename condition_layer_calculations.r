### Calculate conservation opportunity metric for action/target combinations:

library(raster)
library(sf)

infolder <- "C:/Work/SpatialData/NFWF_Cross_Realm/Final Input Rasters/"  # folder holding prepped input rasters (should have same projection, resolution, and origin as HUC12 zones layer)
outfolder <- "C:/Work/SpatialData/NFWF_Cross_Realm/Final Condition Rasters/"   # folder where condition raster will be written



# load HUC12 zones
zones <- raster(paste0(infolder, "HUC12_zones_30m_NAD83UTM.tif"))
zones.10m <- raster::disaggregate(zones, fact=3, method="", filename=paste0(infolder, "HUC12_zones_10m_NAD83UTM.tif"))
zones.90m <- raster::aggregate(zones, fact=3, fun=modal, filename=paste0(infolder, "HUC12_zones_90m_NAD83UTM.tif"))
zones.270m <- raster::aggregate(zones, fact=9, fun=modal, filename=paste0(infolder, "HUC12_zones_270m_NAD83UTM.tif"))
#num.zones <- cellStats(zones, "max")
#huc12.areas <- as.data.frame(freq(zones, digits=0, value=NULL, useNA="no", progress="window", merge=TRUE))  # count of cells within each HUC12 unit

#valley.test <- setExtent(valley.10m, zones, keepres=TRUE, snap=TRUE)

########################################################################################################################
### INPUT LAYER PROCESSING ############################################################################################
########################################################################################################################

# Import 10-m versions of input layers to allow analysis with finest scale valley bottoms and streams layers (and crop to 10-m zones extent)
valley.10m <- raster(paste0(infolder, "Theobald_valley_bottom_10m_NAD83UTM.tif"))
stream.biotic.10m <- raster(paste0(infolder,"stream_biotic_condition_10m_NAD83UTM.tif"))
stream.cfm.10m <- raster(paste0(infolder,"stream_CFM_shift_10m_NAD83UTM.tif"))
sinuosity.10m <- raster(paste0(infolder,"stream_sinuosity_10m_NAD83UTM.tif"))
roads.10m <- raster(paste0(infolder,"decommissionable_roads_10m_NAD83UTM.tif"))

# Disaggregate coarser-scale inputs to 10-m to allow raster operations with stream and valley bottom layers
irrig.90mbuffer <- raster(paste0(infolder, "irrigated_lands_90m_buffer_NAD83UTM.tif"))
irrig.90mbuffer.10m <- raster::disaggregate(irrig.90mbuffer, fact=3, method="", filename=paste0(infolder, "irrigated_lands_90m_buffer_10m_NAD83UTM.tif"))
grazing.90mbuffer <- raster(paste0(infolder, "public_grazing_lands_90m_buffer_NAD83UTM.tif"))
grazing.90mbuffer.10m <- raster::disaggregate(grazing.90mbuffer, fact=3, method="", filename=paste0(infolder, "public_grazing_lands_90m_buffer_10m_NAD83UTM.tif"))
agriculture.90mbuffer <- raster(paste0(infolder, "agricultural_land_90m_buffer_NAD83UTM.tif"))
agriculture.90mbuffer.10m <- raster::disaggregate(agriculture.90mbuffer, fact=3, method="", filename=paste0(infolder, "agricultural_land_90m_buffer_10m_NAD83UTM.tif"))
forest.centrality <- raster(paste0(infolder,"forest_centrality_flowlines_NAD83UTM.tif"))
forest.centrality.10m <- raster::disaggregate(forest.centrality, fact=3, method="", filename=paste0(infolder, "forest_centrality_flowlines_10m_NAD83UTM.tif"))
grassland.centrality <- raster(paste0(infolder,"grassland_centrality_flowlines_NAD83UTM.tif"))
grassland.centrality.10m <- raster::disaggregate(grassland.centrality, fact=3, method="", filename=paste0(infolder, "grassland_centrality_flowlines_10m_NAD83UTM.tif"))
shrubland.centrality <- raster(paste0(infolder,"shrubland_centrality_flowlines_NAD83UTM.tif"))
shrubland.centrality.10m <- raster::disaggregate(shrubland.centrality, fact=3, method="", filename=paste0(infolder, "shrubland_centrality_flowlines_10m_NAD83UTM.tif"))
waterbody <- raster(paste0(infolder,"lakes_ponds_reservoirs_NAD83UTM.tif"))# load in water bodies
waterbody.10m <- raster::disaggregate(waterbody, fact=3, method="", filename=paste0(infolder, "lakes_ponds_reservoirs_10m_NAD83UTM.tif"))
hmi <- raster(paste0(infolder,"HMI_90m_NAD83UTM.tif")) # load in hmi
hmi.10m <- raster::disaggregate(hmi, fact=3, method="", filename=paste0(infolder, "HMI_10m_NAD83UTM.tif"))
bridge.buffer <- raster(paste0(infolder, "bridges_1km_buffer_NAD83UTM.tif"))
bridge.buffer.10m <- raster::disaggregate(bridge.buffer, fact=3, method="", filename=paste0(infolder, "bridges_1km_buffer_10m_NAD83UTM.tif"))
roads.1kmbuffer <- raster(paste0(infolder, "decommissionable_roads_1km_buffer_NAD83UTM.tif")) # read in buffered decommissionable roads layer
roads.1kmbuffer.10m <- raster::disaggregate(roads.1kmbuffer, fact=3, method="", filename=paste0(infolder, "decommissionable_roads_1km_buffer_10m_NAD83UTM.tif"))
privatecons <- raster(paste0(infolder, "conservation_easement_mask_NAD83UTM.tif"))
privatecons.10m <- raster::disaggregate(privatecons, fact=3, method="", filename=paste0(infolder, "conservation_easement_mask_10m_NAD83UTM.tif"))
fire.hazard <- raster(paste0(infolder, "wildfire_hazard_potential_categorical_NAD83UTM.tif"))
fire.hazard.10m <- raster::disaggregate(fire.hazard, fact=3, method="", filename=paste0(infolder, "wildfire_hazard_potential_categorical_10m_NAD83UTM.tif"))


# reformat valley bottoms layer to be binary and exclude water bodies
vb.reclass.mat <- matrix(c(0,NA,1,1,2,1,3,1,4,1,5,1,6,1,7,1,8,1,9,1), ncol=2, byrow=TRUE)
vb.temp <- reclassify(valley.10m, vb.reclass.mat)  
watermask <- waterbody.10m
waterbody.reclass.mat <- matrix(c(NA, 1, 1, NA), nrow=2, ncol=2, byrow=TRUE)
watermask <- reclassify(waterbody.10m, waterbody.reclass.mat)
vb.nowaterbody <- vb.temp * watermask
writeRaster(vb.nowaterbody, filename=paste0(infolder, "valley_bottom_no_waterbody_10m_NAD83UTM.tif"), overwrite=TRUE)

# create uplands layer (everything except valley bottoms and water bodies)
uplands.reclass.mat <- matrix(c(NA, 1, 1, NA), ncol=2, byrow=TRUE)
uplands.temp <- reclassify(vb.nowaterbody, uplands.reclass.mat)
uplands.10m <- uplands.temp * watermask * MHB.10m
writeRaster(uplands.10m, filename=paste0(infolder, "uplands_10m_NAD83UTM.tif"), overwrite=TRUE)

# reformat wildfire hazard layer to be binary (1=high or very  high, NA=lesser risk)
fire.reclass.mat <- matrix(c(1, NA, 2, NA, 3, NA, 4, 1, 5, 1, 6, NA, 7, NA), nrow=7, ncol=2, byrow=TRUE)
fire.highrisk.10m <- reclassify(fire.hazard.10m, fire.reclass.mat, filename=paste0(infolder, "wildfire_hazard_high_and_veryhigh_10m_NAD83UTM.tif"))


MHB.10m <- reclassify(zones.10m, matrix(c(1,413,1), nrow=1, byrow=1))
writeRaster(MHB.10m, filename=paste0(infolder, "MHB_10m.tif"))


# Create reversed version of stream biotic condition where poorer condition corresponds to higher value
stream.biotic <- raster(paste0(infolder,"stream_biotic_condition_10m_NAD83UTM.tif"))  # import stream biotic condition layer
minval <- cellStats(stream.biotic, stat="min")    
maxval <- cellStats(stream.biotic, stat="max") 
reversed.stream.biotic <- 1 - ((stream.biotic - minval)/(maxval-minval)) # reverse scaling of biotic condition score so poorest condition has highest value
writeRaster(reversed.stream.biotic, filename=paste0(infolder, "reversed_stream_biotic_condition_10m_NAD83UTM.tif"))

# Create new version of stream CFM shift
stream.cfm <- raster(paste0(infolder,"stream_CFM_shift_10m_NAD83UTM.tif"))  # import stream cfm layer (value represents shift in Julian day of CFM - all negative values because all shifts are predicted to be earlier)
cfm.days.early <- abs(stream.cfm) # convert to positive values - now values represent the number of days by which CFM is predicted to come earlier in the year
writeRaster(cfm.days.early, paste0(infolder,"stream_CFM_days_early_10m_NAD83UTM.tif"))
minval <- cellStats(cfm.days.early, stat="min")
maxval <- cellStats(cfm.days.early, stat="max")
reversed.cfm.days.early <- 1 - ((cfm.days.early - minval)/(maxval-minval)) # reverse scaling so that higher values = smallest predicted shift earlier = highest conservation value
writeRaster(reversed.cfm.days.early, paste0(infolder,"reversed_stream_CFM_days_early_10m_NAD83UTM.tif"))





save.image("C:/Work/SpatialData/NFWF_Cross_Realm/NFWF_raster_inputs_10m.RData")


##############################################################################################################################################
load("C:/Work/SpatialData/NFWF_Cross_Realm/NFWF_raster_inputs_10m.RData")



# Need to replace the script below - we now evaluate mean values within a particular action areas, not for MHB as a whole

# Biotic condition
mean.stream.biotic.10m <- cellStats(stream.biotic.10m, stat="mean", na.rm=TRUE)  # calculate mean value for assigning threshold
good.stream.biotic.10m <- stream.biotic.10m  # create new copy of stream biotic condition for high quality streams only (i.e., those with high conservation value)
good.stream.biotic.10m[good.stream.biotic.10m < mean.stream.biotic.10m] <- NA # assign NA value to all stream pixels below this condition to keep only streams that it makes sense to conserve
bad.stream.biotic.10m <- stream.biotic.10m # create new copy of stream biotic condition for low quality streams only (i.e., those with high restoration value)
bad.stream.biotic.10m[bad.stream.biotic.10m > mean.stream.biotic.10m] <- NA # assign NA value to all stream pixels above this condition to keep only streams that it makes sense to restore

# CFM shift
min.cfm.10m <- cellStats(stream.cfm.10m, stat="min")
pos.stream.cfm.10m <- stream.cfm.10m + abs(min.cfm.10m)  # switch to positive values (higher = more resistant to early shift in CFM)
mean.pos.stream.cfm.10m <- cellStats(pos.stream.cfm.10m, stat="mean", na.rm=TRUE)  # calculate mean value for assigning threshold
good.stream.cfm.10m <- pos.stream.cfm.10m  # create new copy of stream CFM shift for good streams only (i.e., those with low predicted shift in CFM and therefore high conservation value)
good.stream.cfm.10m[good.stream.cfm.10m < mean.pos.stream.cfm.10m] <- NA # assign NA value to all stream pixels with early shift that is larger than average to keep only streams that it makes sense to conserve
bad.stream.cfm.10m <- pos.stream.cfm.10m # create new copy of stream biotic condition for low quality streams only (i.e., those with high restoration value)
bad.stream.cfm.10m[bad.stream.cfm.10m > mean.pos.stream.cfm.10m] <- NA # assign NA value to all stream pixels with early shift that is smaller than average to keep only streams that it makes sense to restore



  
    
  
  
# Will want to save as .RData file here so I don't have to reprocess input layers every time
save()



########################################################################################################################
### FOREST/SHRUBLAND FUELS MANAGEMENT
########################################################################################################################

### Fuels mgmt --> riparian configuration
vb <- raster(paste0(infolder, "valley_bottom_no_waterbody_10m_NAD83UTM.tif"))  # import valley bottoms layer
fire <- raster(paste0(infolder, "wildfire_hazard_high_and_veryhigh_10m_NAD83UTM.tif"))   # import wildfire risk layer
lowrisk.vb <- vb * fire   # keep only those pixels that are both high/very high fire risk and valley bottom
zonal.mat <- zonal(x=lowrisk.vb, z=zones.10m, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit (index of area)
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "fuelsMgmt_riparianConfig_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Fuels mgmt --> Upland veg
uplands <- raster(paste0(infolder, "uplands_10m_NAD83UTM.tif"))  # import uplands layer
fire <- raster(paste0(infolder, "wildfire_hazard_high_and_veryhigh_10m_NAD83UTM.tif"))   # import wildfire risk layer
lowrisk.uplands <- uplands * fire   # keep only those pixels that are both high/very high fire risk and uplands
zonal.mat <- zonal(x=lowrisk.uplands, z=zones.10m, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit (index of area)
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "fuelsMgmt_uplandVeg_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


### Fuels mgmt --> Upland veg
uplands <- raster(paste0(infolder, "uplands_10m_NAD83UTM.tif"))  # import uplands layer
fire <- raster(paste0(infolder, "wildfire_hazard_high_and_veryhigh_10m_NAD83UTM.tif"))   # import wildfire risk layer
lowrisk.uplands <- uplands * fire   # keep only those pixels that are both high/very high fire risk and uplands
zonal.mat <- zonal(x=lowrisk.uplands, z=zones.10m, fun="sum", na.rm=TRUE)   # sum remaining pixels in each HUC unit (index of area)
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for action
original <- zonal.mat[,2] # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "fuelsMgmt_uplandVeg_condition_NAD83UTM.tif"  # name of output
writeRaster(condition, paste0(outfolder, outfilename))  # write output
rm(list=setdiff(ls(), c("zones.10m", "infolder", "outfolder")))  # remove all R objects except for infolder, outfolder, and zones.10m
gc() # garbage collector to free up memory


  
########################################################################################################################
### LAND PROTECTION ####################################################################################################
########################################################################################################################

### LAND PROTECTION --> STREAM HEALTH
# target HUCs with lots of streams in good biotic condition
# condition value is sum of good stream biotic condition values divided by HUC area
zonal.mat <- zonal(x=good.stream.biotic, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for good streams in each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for stream conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1; we want to prioritize units with the highest biotic condition to conserve
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_streamhealth_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))



### LAND PROTECTION --> NORMATIVE FLOW REGIME
# target HUCs with lots of streams that are resistant to shift toward earlier CFM
# condition is sum of good stream CFM shift divided by HUC area 
zonal.mat <- zonal(x=good.stream.cfm, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of cfm values for each zone
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw cfm values (sum cfm shift / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_flowregime_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))



### LAND PROTECTION --> WITHIN-HABITAT CONNECTIVITY
# target HUCs that have lots of high-centrality flowlines

  # FOREST: condition is sum of centrality flowline values divided by HUC area
  zonal.mat <- zonal(x=forest.centrality, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
  zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for conservation
  original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
  rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
  reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
  condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
  outfilename <- "landpro_forest_centrality_condition_SUM.tif"
  writeRaster(condition, paste0(outfolder, outfilename))
  
  # GRASSLAND: condition is sum of centrality flowline values divided by HUC area
  grassland.centrality <- raster(paste0(infolder,"grassland_centrality_flowlines_NAD83UTM.tif"))   # read in CFM shift layer
  zonal.mat <- zonal(x=grassland.centrality, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
  zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for conservation
  original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
  rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
  reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
  condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
  outfilename <- "landpro_grassland_centrality_condition_SUM.tif"
  writeRaster(condition, paste0(outfolder, outfilename))
  
  # SHRUBLAND: condition is sum of centrality flowline values divided by HUC area
  shrubland.centrality <- raster(paste0(infolder,"shrubland_centrality_flowlines_NAD83UTM.tif"))   # read in CFM shift layer
  zonal.mat <- zonal(x=shrubland.centrality, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
  zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for conservation
  original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
  rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
  reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
  condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
  outfilename <- "landpro_shrubland_centrality_condition_SUM.tif"
  writeRaster(condition, paste0(outfolder, outfilename))


### LAND PROTECTION --> RIPARIAN CONFIGURATION
# target HUCs that have lots of minimally modified (HMI<0.2) valley bottom and high stream sinuosity??
# conditions are (1) amount of undeveloped valley bottom and (2) mean stream sinuosity
valley <- raster(paste0(infolder,"Theobald_valley_bottom_30m_NAD83UTM.tif")) # load in valley bottoms
valley[valley==0] <- NA
valley[valley>0] <- 1
water <- raster(paste0(infolder,"lakes_ponds_reservoirs_NAD83UTM.tif"))# load in water bodies
water[is.na(water)==TRUE] <- 0
water[water==1] <- NA
water[water==0] <- 1
hmi <- raster(paste0(infolder,"HMI_90m_NAD83UTM.tif")) # load in hmi
hmi <- crop(hmi, zones)
hmi[hmi>0.2] <- NA
hmi[hmi<=0.2] <- 1
lowmod.valley <- valley * water * hmi # hmi in valley bottoms
zonal.mat1 <- zonal(x=lowmod.valley, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
zonal.mat1[zonal.mat1==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for conservation
original1 <- zonal.mat1[,2]/huc12.areas[,2]  # vector of raw condition values 
rescaled1 <- (original1 - min(original1, na.rm=TRUE))/(max(original1, na.rm=TRUE)-min(original1, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
sinuosity <- raster(paste0(infolder, "stream_sinuosity_NAD83UTM.tif"))
sinuosity.valley <- sinuosity * valley
zonal.mat2 <- zonal(x=sinuosity.valley, z=zones, fun="mean", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
zonal.mat2[zonal.mat2==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for conservation
original2 <- zonal.mat2[,2]  # vector of raw condition values 
rescaled2 <- (original2 - min(original2, na.rm=TRUE))/(max(original2, na.rm=TRUE)-min(original2, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
combo <- rescaled1 + rescaled2
rescaled.combo <- (combo - min(combo, na.rm=TRUE))/(max(combo, na.rm=TRUE)-min(combo, na.rm=TRUE))
reclass.mat <- as.matrix(cbind(zonal.mat1[,1],rescaled.combo))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_riparianconfig_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))



### LAND PROTECTION --> UPLAND VEG COMPOSITION/STRUCTURE
# target HUCs with lots of minimally modified upland
# condition is amount of terrestrial upland with HMI<0.2
uplands <- raster(paste0(infolder,"Theobald_valley_bottom_30m_NAD83UTM.tif")) # load in valley bottoms to convert to uplands layer
uplands[uplands>0] <- NA
uplands[uplands==0] <- 1
nonwater <- raster(paste0(infolder,"lakes_ponds_reservoirs_NAD83UTM.tif"))# load in water bodies and convert to non-water body
nonwater[is.na(nonwater)==TRUE] <- 0
nonwater[nonwater==1] <- NA
nonwater[nonwater==0] <- 1
upland.terrestrial <- uplands * nonwater # multiply to get area of terrestrial uplands
hmi <- raster(paste0(infolder,"HMI_90m_NAD83UTM.tif")) # load in hmi
hmi <- crop(hmi, zones)
hmi[hmi>0.2] <- NA
hmi[hmi<=0.2] <- 1
lowmod.uplands <- upland.terrestrial * hmi # multiply to get area of minimally modified terrestrial uplands
zonal.mat <- zonal(x=lowmod.uplands, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_uplandveg_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))






########################################################################################################################
### SOIL HEALTH MANAGEMENT #############################################################################################
########################################################################################################################



### SOIL HEALTH MANAGEMENT --> STREAM HEALTH
# condition is 

### SOIL HEALTH MANAGEMENT --> NORMATIVE FLOW REGIME



### SOIL HEALTH MANAGEMENT --> RIPARIAN CONFIGURATION




########################################################################################################################
### PRESCRIBED FIRE/THINNING ###########################################################################################
########################################################################################################################



### PRESCRIBED FIRE/THINNING --> NORMATIVE FLOW REGIME

### PRESCRIBED FIRE/THINNING --> WITHIN-HABITAT CONNECTIVITY

### PRESCRIBED FIRE/THINNING --> NORMATIVE FLOW REGIME

### PRESCRIBED FIRE/THINNING --> RIPARIAN CONFIGURATION





########################################################################################################################
### STREAM/RIPARIAN RESTORATION ########################################################################################
########################################################################################################################


### STREAM/RIPARIAN RESTORATION --> STREAM HEALTH
# condition is sum of stream biotic condition (inverted so that worse condition score = higher value) divided by HUC area
minval <- cellStats(bad.stream.biotic, stat="min")    # reverse biotic condition score for bad streams
maxval <- cellStats(bad.stream.biotic, stat="max")
rescaled.bad.stream.biotic <- 1 - ((bad.stream.biotic - minval)/(maxval-minval))
zonal.mat <- zonal(x=rescaled.bad.stream.biotic, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for good streams in each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for stream conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1; we want to prioritize units with the highest biotic condition to conserve
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "streamriprest_streamhealth_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))


### STREAM/RIPARIAN RESTORATION --> NORMATIVE FLOW REGIME
# condition is sum of cfm shift (inverted so that greater early shift = higher value) divided by HUC area
minval <- cellStats(bad.stream.cfm, stat="min")    # reverse cfm shift for bad streams
maxval <- cellStats(bad.stream.cfm, stat="max")
rescaled.bad.stream.cfm <- 1 - ((bad.stream.cfm - minval)/(maxval-minval))
zonal.mat <- zonal(x=rescaled.bad.stream.cfm, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for good streams in each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for stream conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1; we want to prioritize units with the highest biotic condition to conserve
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "streamriprest_flowregime_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))


### STREAM/RIPARIAN RESTORATION --> WITHIN-HABITAT CONNECTIVITY
# condition is sum of centrality flowlines in undeveloped valley bottoms divided by HUC area
valley <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Removal Masks NAD83 UTM/stream_riparian_restoration_removal_mask_NAD83UTM.tif")     # bring in valley bottom mask
summed.centrality <- raster(paste0(infolder,"summed_centrality_flowlines_NAD83UTM.tif"))   # read in all biome centrality layer
valley.centrality <- valley * summed.centrality   # multiply to get centrality values for flowlines within undeveloped valley bottoms
zonal.mat <- zonal(x=valley.centrality, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for good streams in each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for stream conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1; we want to prioritize units with the highest biotic condition to conserve
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "streamriprest_summed_centrality_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))


### STREAM/RIPARIAN RESTORATION --> RIPARIAN CONFIGURATION
# target HUCs that have lots undeveloped valley bottom with high HMI (>0.2) and low stream sinuosity??
# conditions are (1) amount of undeveloped valley bottom and (2) mean stream sinuosity
valley <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Removal Masks NAD83 UTM/stream_riparian_restoration_removal_mask_NAD83UTM.tif")     # bring in valley bottom mask (undeveloped, non-water)
hmi <- raster(paste0(infolder,"HMI_90m_NAD83UTM.tif")) # load in hmi
hmi <- crop(hmi, zones)
hmi[hmi<0.2] <- NA
hmi[hmi>=0.2] <- 1
highmod.valley <- valley * hmi # areas of undeveloped valley bottom that are more modified
zonal.mat1 <- zonal(x=highmod.valley, z=zones, fun="sum", na.rm=TRUE)  # calculate area within each HUC
zonal.mat1[zonal.mat1==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for conservation
original1 <- zonal.mat1[,2]/huc12.areas[,2]  # vector of raw condition values 
rescaled1 <- (original1 - min(original1, na.rm=TRUE))/(max(original1, na.rm=TRUE)-min(original1, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
sinuosity <- raster(paste0(infolder, "stream_sinuosity_NAD83UTM.tif"))
sinuosity.valley <- sinuosity * highmod.valley
zonal.mat2 <- zonal(x=sinuosity.valley, z=zones, fun="mean", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
zonal.mat2[zonal.mat2==0 | zonal.mat2=="NaN"] <- NA  
original2 <- zonal.mat2[,2]  # vector of raw condition values 
rescaled2 <- 1 - (original2 - min(original2, na.rm=TRUE))/(max(original2, na.rm=TRUE)-min(original2, na.rm=TRUE))  # NEGATIVE RELATIONSHIP rescale condition values from 0-1
combo <- rescaled1 + rescaled2
rescaled.combo <- (combo - min(combo, na.rm=TRUE))/(max(combo, na.rm=TRUE)-min(combo, na.rm=TRUE))
reclass.mat <- as.matrix(cbind(zonal.mat1[,1],rescaled.combo))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "streamriprest_riparianconfig_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))




########################################################################################################################
### BRIDGE/CULVERT UPGRADE #############################################################################################
########################################################################################################################

# load in sf layer of bridges
# buffer by 1k radius
bridges <- st_read()

### BRIDGE/CULVERT UPGRADE --> STREAM HEALTH   # calculate stream biotic condition within buffered area




### BRIDGE/CULVERT UPGRADE --> WITHIN-HABITAT CONNECTIVITY   # calculate maximum connectivity value within buffered area
# target HUCs with bridges needing upgrading that are near centrality flowlines
# condition is sum of centrality flowline values within buffer of bridges
bridge.buffer <- raster(paste0(infolder, "bridges_1km_buffer_NAD83UTM.tif"))
summed.centrality <- raster(paste0(infolder,"summed_centrality_flowlines_NAD83UTM.tif"))   # read in CFM shift layer
buffer.centrality <- bridge.buffer * summed.centrality  # multiple bridges and centrality to remove flowlines from outside buffers
zonal.mat <- zonal(x=buffer.centrality, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "bridgeculvert_summed_centrality_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))





########################################################################################################################
### ROAD DECOMMISSIONING ###############################################################################################
########################################################################################################################


### ROAD DECOMMISSIONING --> STREAM HEALTH   
# target areas that have lots of streams in poor health within a buffer of decommissionable roads
minval <- cellStats(bad.stream.biotic, stat="min")    # reverse biotic condition score for bad streams
maxval <- cellStats(bad.stream.biotic, stat="max")
rescaled.bad.stream.biotic <- 1 - ((bad.stream.biotic - minval)/(maxval-minval))
bad.stream.road.buffer <- roads * rescaled.bad.stream.biotic
zonal.mat <- zonal(x=bad.stream.road.buffer, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for good streams in each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for stream conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1; we want to prioritize units with the highest biotic condition to conserve
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "roaddecommission_streamhealth_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))


### ROAD DECOMMISSIONING --> WITHIN-HABITAT CONNECTIVITY
# target HUCS with lots of centrality flowlines near roads
roads <- raster(paste0(infolder, "decommissionable_roads_1km_buffer_NAD83UTM.tif")) # read in buffered decommissionable roads layer
roads[is.na(roads)==FALSE] <- 1
roads <- crop(roads,zones)
buffer.centrality <- roads * summed.centrality  # multiple bridges and centrality to remove flowlines from outside buffers
zonal.mat <- zonal(x=buffer.centrality, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
zonal.mat[zonal.mat==0] <- NA  
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "roaddecommission_summed_centrality_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))


### ROAD DECOMMISSIONING --> RIPARIAN CONFIGURATION
# target HUCS with lots of decommissionable roads within valley bottoms
valley <- raster(paste0(infolder,"Theobald_valley_bottom_30m_NAD83UTM.tif"))
valley[valley>0] <- 1
valley[valley==0] <- NA
roads <- raster(paste0(infolder, "decommissionable_roads_NAD83UTM.tif")) # read in buffered decommissionable roads layer
roads[is.na(roads)==FALSE] <- 1
roads <- crop(roads,zones)
valley.roads <- valley * roads
zonal.mat <- zonal(x=valley.roads, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
zonal.mat[zonal.mat==0] <- NA  
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "roaddecommission_riparianconfig_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))


### ROAD DECOMMISSIONING --> UPLAND VEG COMPOSITION/STRUCTURE





########################################################################################################################
### GRAZING MANAGEMENT #################################################################################################
########################################################################################################################
grazing.areas <- as.data.frame(zonal(x=grazing.land, z=zones, fun="count", digits=0, na.rm=TRUE))  # Calculate remaining area within each HUC
grazing.proportions <- grazing.areas$count/huc12.areas$count  # Calculate proportion available in each HUC12 unit (remaining area/HUC area)

# create new zones layer specific to landpro by assigning NA to all cells that are not within landpro mask
grazing.zones <- overlay(zones, grazing.land, fun=function(x,y) {
  x[is.na(y[])] <- NA
  return(x)
})




### GRAZING MANAGEMENT --> STREAM HEALTH
# condition is sum of 

### GRAZING MANAGEMENT --> WITHIN-HABITAT CONNECTIVITY

### GRAZING MANAGEMENT --> RIPARIAN CONFIGURATION

### GRAZING MANAGEMENT --> UPLAND VEG COMPOSITION/STRUCTURE





########################################################################################################################
### IRRIGATION ADJUSTMENT ##############################################################################################
########################################################################################################################

### IRRIGATION ADJUSTMENT --> STREAM HEALTH
# condition is sum of stream biotic condition (inverted so that worse condition score = higher value) divided by HUC area
minval <- cellStats(bad.stream.biotic, stat="min")    # reverse biotic condition score for bad streams
maxval <- cellStats(bad.stream.biotic, stat="max")
rescaled.bad.stream.biotic <- 1 - ((bad.stream.biotic - minval)/(maxval-minval))
zonal.mat <- zonal(x=rescaled.bad.stream.biotic, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for good streams in each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for stream conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1; we want to prioritize units with the highest biotic condition to conserve
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "irrigadjust_streamhealth_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))


### IRRIGATION ADJUSTMENT --> NORMATIVE FLOW REGIME
# condition is sum of cfm shift (inverted so that greater early shift = higher value) divided by HUC area
minval <- cellStats(bad.stream.cfm, stat="min")    # reverse cfm shift for bad streams
maxval <- cellStats(bad.stream.cfm, stat="max")
rescaled.bad.stream.cfm <- 1 - ((bad.stream.cfm - minval)/(maxval-minval))
zonal.mat <- zonal(x=rescaled.bad.stream.cfm, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for good streams in each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for stream conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1; we want to prioritize units with the highest biotic condition to conserve
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "irrigadjust_flowregime_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))













######### EXTRAS



# Problem here - don't want to assign NaN or -Inf as zeros, because then they will be highest value (since CFM shift values are negative)
# Could assign values AFTER rescaling?
zonal.mat[zonal.mat=="-Inf" | zonal.mat=="NaN"] <- 0  # replace -Inf values with zeros (or NA? Not sure?)
missingzones <- setdiff(c(1:num.zones), zonal.mat[,1])  # find zones that were not included in zonal stats because proportion = 0
full.zonal.mat <- data.frame(zone=c(zonal.mat[,1], missingzones), value=c(zonal.mat[,2], rep(0, length(missingzones))))
full.zonal.mat <- full.zonal.mat[order(full.zonal.mat$zone),]
original <- full.zonal.mat[,2]
rescaled <- (original - min(original))/(max(original)-min(original))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
opportunity <- rescaled*landpro.proportions
reclass.mat <- as.matrix(cbind(full.zonal.mat[,1],opportunity))  # create reclassification table
opportunity <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_streamhealth_opportunity.tif"
