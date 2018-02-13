### Calculate conservation opportunity metric for action/target combinations:

library(raster)
library(sf)

infolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/"  # folder holding prepped input rasters (should have same projection, resolution, and origin as HUC12 zones layer)
outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Opportunity Layers/"   # folder where condition raster will be written

# load HUC12 zones
zones <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/HUC12_zones_30m.tif")
num.zones <- cellStats(zones, "max")
huc12.areas <- as.data.frame(freq(zones, digits=0, value=NULL, useNA="no", progress="window", merge=TRUE))  # count of cells within each HUC12 unit

# load target variables
alpine.centrality <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/alpine_centrality_flowlines.tif")
forest.centrality <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/forest_centrality_flowlines.tif")
grassland.centrality <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/grassland_centrality_flowlines.tif")
shrubland.centrality <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/shrubland_centrality_flowlines.tif")
soil.water <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/root_zone_available_water_storage.tif")
stream.biotic <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/stream_biotic_condition.tif")
sinuosity <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/stream_sinuosity.tif")
stream.temp.pc <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/stream_temperature_2040_percent_change.tif")
stream.flow.cmf.shift <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/streamflow_CFM_2040_shift.tif")
streamflow.annual.pc <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/streamflow_mean_annual_2040_percent_change.tif")
road.class <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/TIGER_roads_mtfcc.tif")
soil.carbon <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/total_profile_soil_organic_carbon.tif")
fire.hazard <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/wildfire_hazard_potential.tif")

# load masks
unprotected.land <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/conservation_easement_mask.tif")  
irrigated.land <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/irrigated_land.tif")
grazing.land <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/public_grazing_lands.tif")
valley.bottom.10m <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/Theobald_valley_bottom_10m.tif")
valley.bottom.30m <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/Theobald_valley_bottom_30m.tif")






########################################################################################################################
### SOIL HEALTH MANAGEMENT #############################################################################################
########################################################################################################################



### SOIL HEALTH MANAGEMENT --> STREAM HEALTH

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
### LAND PROTECTION ####################################################################################################
########################################################################################################################
landpro.areas <- as.data.frame(zonal(x=unprotected.land, z=zones, fun="count", digits=0, na.rm=TRUE))  # Calculate remaining area within each HUC
landpro.proportions <- landpro.areas$count/huc12.areas$count  # Calculate proportion available in each HUC12 unit (remaining area/HUC area)

# create new zones layer specific to landpro by assigning NA to all cells that are not within landpro mask
landpro.zones <- overlay(zones, unprotected.land, fun=function(x,y) {
  x[is.na(y[])] <- NA
  return(x)
})

### LAND PROTECTION --> STREAM HEALTH
zonal.mat <- zonal(x=stream.biotic, z=landpro.zones, fun="mean", digits=3, na.rm=TRUE)  # calculate zonal stats for stream biotic condition
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
writeRaster(opportunity, paste0(outfolder, outfilename))

### LAND PROTECTION --> NORMATIVE FLOW REGIME
zonal.mat <- zonal(x=stream.flow.cmf.shift, z=landpro.zones, fun="mean", na.rm=TRUE)  # calculate zonal stats for normative flow


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
writeRaster(opportunity, paste0(outfolder, outfilename))



### LAND PROTECTION --> WITHIN-HABITAT CONNECTIVITY
zonal.mat <- zonal(x=stream.biotic, z=landpro.zones, fun="mean", digits=3, na.rm=TRUE)  # calculate zonal stats for stream biotic condition
zonal.mat[zonal.mat=="-Inf" | zonal.mat=="NaN"] <- 0  # replace -Inf values with zeros (or NA? Not sure?)
missingzones <- setdiff(c(1:num.zones), zonal.mat[,1])  # find zones that were not included in zonal stats because proportion = 0
full.zonal.mat <- data.frame(zone=c(zonal.mat[,1], missingzones), value=c(zonal.mat[,2], rep(0, length(missingzones))))
full.zonal.mat <- full.zonal.mat[order(full.zonal.mat$zone),]
original <- full.zonal.mat[,2]
rescaled <- (original - min(original))/(max(original)-min(original))  # POSITIVE RELATIONSHIP rescale condition values from 0-1
opportunity <- rescaled*landpro.proportions
reclass.mat <- as.matrix(cbind(full.zonal.mat[,1],opportunity))  # create reclassification table
opportunity <- reclassify(zones, reclass.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
outfilename <- "landpro_connectivity_opportunity.tif"
writeRaster(opportunity, paste0(outfolder, outfilename))

### LAND PROTECTION --> RIPARIAN CONFIGURATION
landpro.riparian <- 


### LAND PROTECTION --> UPLAND VEG COMPOSITION/STRUCTURE
landpro.uplandveg <- 




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

