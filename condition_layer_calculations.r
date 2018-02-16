### Calculate conservation opportunity metric for action/target combinations:

library(raster)
library(sf)

infolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs NAD83 UTM/"  # folder holding prepped input rasters (should have same projection, resolution, and origin as HUC12 zones layer)
outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Condition Rasters NAD83 UTM/"   # folder where condition raster will be written
#outfolder <- "C:/Users/Tyler/Desktop/Condition rasters/"


# load HUC12 zones
zones <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs NAD83 UTM/HUC12_zones_30m_NAD83UTM.tif")
num.zones <- cellStats(zones, "max")
huc12.areas <- as.data.frame(freq(zones, digits=0, value=NULL, useNA="no", progress="window", merge=TRUE))  # count of cells within each HUC12 unit



########################################################################################################################
### LAND PROTECTION ####################################################################################################
########################################################################################################################

### LAND PROTECTION --> STREAM HEALTH
# condition value is sum of stream biotic condition values divided by HUC area
stream.biotic <- raster(paste0(infolder,"stream_biotic_condition_NAD83UTM.tif"))   # read in stream biotic condition layer
zonal.mat <- zonal(x=stream.biotic, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
zonal.mat[zonal.mat==0] <- NA  # set zones with sum=0 to NA, since we don't want to include these in prioritization as they have no opportunity for stream conservation
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1; we want to prioritize units with the highest biotic condition to conserve
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_streamhealth_condition.tif"
writeRaster(condition, paste0(outfolder, outfilename))

# condition is mean CFM shift within HUC
stream.biotic <- raster(paste0(infolder,"stream_biotic_condition_NAD83UTM.tif"))   # read in stream biotic condition layer
zonal.mat <- zonal(x=stream.biotic, z=zones, fun="mean", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
zonal.mat[zonal.mat=="NaN"] <- NA  # set zones with no calculable mean to NA, since we don't want to include these in prioritization as they have no opportunity for stream conservation
original <- zonal.mat[,2]  # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1; we want to prioritize units with the highest biotic condition to conserve
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_streamhealth_condition_MEAN.tif"
writeRaster(condition, paste0(outfolder, outfilename))




### LAND PROTECTION --> NORMATIVE FLOW REGIME
# condition is sum of CFM shift divided by HUC area 
stream.flow.cfm.shift <- raster(paste0(infolder,"streamflow_CFM_2040_shift_NAD83UTM.tif"))   # read in CFM shift layer
zonal.mat <- zonal(x=stream.flow.cfm.shift, z=zones, fun="sum", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
original <- zonal.mat[,2]/huc12.areas[,2]  # vector of raw condition values (sum stream biotic condition / HUC area)
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_flowregime_condition_SUM.tif"
writeRaster(condition, paste0(outfolder, outfilename))

# condition is mean value within HUC
stream.flow.cfm.shift <- raster(paste0(infolder,"streamflow_CFM_2040_shift_NAD83UTM.tif"))   # read in CFM shift layer
zonal.mat <- zonal(x=stream.flow.cfm.shift, z=zones, fun="mean", na.rm=TRUE)  # calculate sum of biotic condition values for each zone
original <- zonal.mat[,2]  # vector of raw condition values
rescaled <- (original - min(original, na.rm=TRUE))/(max(original, na.rm=TRUE)-min(original, na.rm=TRUE))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_flowregime_condition_MEAN.tif"
writeRaster(condition, paste0(outfolder, outfilename))



writeRaster(opportunity, paste0(outfolder, outfilename))



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
# condition is total percent of HUC that is unmodified valley bottom
valley <- raster(paste0(infolder, "Theobald_valley_bottom_30m_NAD83UTM.tif"))
valley[valley>0] <- 1
valley[valley==0] <- NA
hmi <- raster(paste0(infolder, "HMI_90m_NAD83UTM.tif"))
hmi[hmi>0.09] <- NA
hmi[hmi<=0.09] <- 1
hmi <- raster::crop(hmi, zones)
undev.valley <- valley * hmi
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
