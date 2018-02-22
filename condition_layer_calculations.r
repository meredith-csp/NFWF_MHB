### Calculate conservation opportunity metric for action/target combinations:

library(raster)
library(sf)

infolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs NAD83 UTM/"  # folder holding prepped input rasters (should have same projection, resolution, and origin as HUC12 zones layer)
#outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Condition Rasters NAD83 UTM/"   # folder where condition raster will be written
outfolder <- "C:/Users/Tyler/Desktop/Condition rasters 2-21-18/"


# load HUC12 zones
zones <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs NAD83 UTM/HUC12_zones_30m_NAD83UTM.tif")
num.zones <- cellStats(zones, "max")
huc12.areas <- as.data.frame(freq(zones, digits=0, value=NULL, useNA="no", progress="window", merge=TRUE))  # count of cells within each HUC12 unit





########################################################################################################################
### TARGET LAYER PROCESSING ############################################################################################
########################################################################################################################

# Stream condition
stream.biotic <- raster(paste0(infolder,"stream_biotic_condition_NAD83UTM.tif"))   # read in stream biotic condition layer
mean.stream.biotic <- cellStats(stream.biotic, stat="mean", na.rm=TRUE)  # calculate mean value for assigning threshold
good.stream.biotic <- stream.biotic  # create new copy of stream biotic condition for high quality streams only (i.e., those with high conservation value)
good.stream.biotic[good.stream.biotic<mean.stream.biotic] <- NA # assign NA value to all stream pixels below this condition to keep only streams that it makes sense to conserve
bad.stream.biotic <- stream.biotic # create new copy of stream biotic condition for low quality streams only (i.e., those with high restoration value)
bad.stream.biotic[bad.stream.biotic>mean.stream.biotic] <- NA # assign NA value to all stream pixels above this condition to keep only streams that it makes sense to restore


# CFM shift
stream.cfm <- raster(paste0(infolder,"streamflow_CFM_2040_shift_NAD83UTM.tif"))   # read in CFM shift layer
min.cfm <- cellStats(stream.cfm, stat="min")
pos.stream.cfm <- stream.cfm + abs(min.cfm)  # switch to positive values (higher = more resistant to early shift in CFM)
mean.pos.stream.cfm <- cellStats(pos.stream.cfm, stat="mean", na.rm=TRUE)  # calculate mean value for assigning threshold
good.stream.cfm <- pos.stream.cfm  # create new copy of stream CFM shift for good streams only (i.e., those with low predicted shift in CFM and therefore high conservation value)
good.stream.cfm[good.stream.cfm<mean.pos.stream.cfm] <- NA # assign NA value to all stream pixels with early shift that is larger than average to keep only streams that it makes sense to conserve
bad.stream.cfm <- pos.stream.cfm # create new copy of stream biotic condition for low quality streams only (i.e., those with high restoration value)
bad.stream.cfm[bad.stream.cfm>mean.pos.stream.cfm] <- NA # assign NA value to all stream pixels with early shift that is smaller than average to keep only streams that it makes sense to restore









########################################################################################################################
### LAND PROTECTION ####################################################################################################
########################################################################################################################

### LAND PROTECTION --> STREAM HEALTH
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
# condition is sum of good stream CFM shift divided by HUC area 
zonal.mat <- zonal(x=good.stream.cfm, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of cfm values for each zone
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw cfm values (sum cfm shift / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_flowregime_condition_NAD83UTM.tif"
writeRaster(condition, paste0(outfolder, outfilename))



### LAND PROTECTION --> WITHIN-HABITAT CONNECTIVITY
# ALL BIOMES (SUMMED): condition is sum of centrality flowline values divided by HUC area
summed.centrality <- raster(paste0(infolder,"summed_centrality_flowlines_NAD83UTM.tif"))   # read in CFM shift layer
zonal.mat <- zonal(x=summed.centrality, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_summed_centrality_condition_SUM.tif"
writeRaster(condition, paste0(outfolder, outfilename))

# FOREST: condition is sum of centrality flowline values divided by HUC area
forest.centrality <- raster(paste0(infolder,"forest_centrality_flowlines_NAD83UTM.tif"))   # read in CFM shift layer
zonal.mat <- zonal(x=forest.centrality, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_forest_centrality_condition_SUM.tif"
writeRaster(condition, paste0(outfolder, outfilename))

# FOREST: condition is sum of centrality flowline values divided by HUC area
alpine.centrality <- raster(paste0(infolder,"alpine_centrality_flowlines_NAD83UTM.tif"))   # read in CFM shift layer
zonal.mat <- zonal(x=alpine.centrality, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_alpine_centrality_condition_SUM.tif"
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
# condition is total length of stream within valley bottom

# load in valley bottoms mask 
# multiply by stream 

zonal.mat <- zonal(x=undev.valley, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_shrubland_centrality_condition_SUM.tif"
writeRaster(condition, paste0(outfolder, outfilename))



    
  
### LAND PROTECTION --> UPLAND VEG COMPOSITION/STRUCTURE
# condition is mean HMI within uplands
# load theobald valley bottoms
# load NHD lakes and reservoirs
# 



upland <- raster(paste0(infolder, "Theobald_valley_bottom_30m_NAD83UTM.tif"))
upland[upland>0] <- NA
upland[upland==0] <- 1
hmi <- raster(paste0(infolder, "HMI_90m_NAD83UTM.tif"))
hmi <- raster::crop(hmi, zones)
upland.hmi <- upland * hmi
zonal.mat <- zonal(x=upland.hmi, z=zones, fun="mean", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
original <- zonal.mat[,2]  # vector of raw condition values
rescaled <- 1 - (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, NA.rm=TRUE)) # NEGATIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_upland_veg_condition_NAD83UTM.tif"
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

### STREAM/RIPARIAN RESTORATION --> NORMATIVE FLOW REGIME

### STREAM/RIPARIAN RESTORATION --> WITHIN-HABITAT CONNECTIVITY

### STREAM/RIPARIAN RESTORATION --> RIPARIAN CONFIGURATION






########################################################################################################################
### BRIDGE/CULVERT UPGRADE #############################################################################################
########################################################################################################################

# load in sf layer of bridges
# buffer by 1k radius
bridges <- st_read()

### BRIDGE/CULVERT UPGRADE --> STREAM HEALTH   # calculate stream biotic condition within buffered area




### BRIDGE/CULVERT UPGRADE --> WITHIN-HABITAT CONNECTIVITY   # calculate maximum connectivity value within buffered area





########################################################################################################################
### ROAD DECOMMISSIONING ###############################################################################################
########################################################################################################################
# calculate number of road miles eligible for decommissioning in each HUC (or buffered area around roads) as proportion measure
# calculate mean target within buffer as the condition metric

### ROAD DECOMMISSIONING --> STREAM HEALTH   # target areas that have lots of roads of correct class and streams in poor health

### ROAD DECOMMISSIONING --> WITHIN-HABITAT CONNECTIVITY

### ROAD DECOMMISSIONING --> RIPARIAN CONFIGURATION

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

### GRAZING MANAGEMENT --> WITHIN-HABITAT CONNECTIVITY

### GRAZING MANAGEMENT --> RIPARIAN CONFIGURATION

### GRAZING MANAGEMENT --> UPLAND VEG COMPOSITION/STRUCTURE





########################################################################################################################
### IRRIGATION ADJUSTMENT ##############################################################################################
########################################################################################################################
irrigation.areas <- as.data.frame(zonal(x=irrigated.land, z=zones, fun="count", digits=0, na.rm=TRUE))  # Calculate remaining area within each HUC
irrigation.proportions <- irrigation.areas$count/huc12.areas$count  # Calculate proportion available in each HUC12 unit (remaining area/HUC area)

# create new zones layer specific to landpro by assigning NA to all cells that are not within landpro mask
irrigation.zones <- overlay(zones, irrigated.land, fun=function(x,y) {
  x[is.na(y[])] <- NA
  return(x)
})

### IRRIGATION ADJUSTMENT --> STREAM HEALTH

### IRRIGATION ADJUSTMENT --> NORMATIVE FLOW REGIME

### IRRIGATION ADJUSTMENT --> UPLAND VEG COMPOSITION/STRUCTURE













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
