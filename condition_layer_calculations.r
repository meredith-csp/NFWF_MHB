### Calculate conservation opportunity metric for action/target combinations:

library(raster)
library(sf)

infolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/"  # folder holding prepped input rasters (should have same projection, resolution, and origin as HUC12 zones layer)
outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Condition Layers/"   # folder where condition raster will be written

# load HUC12 zones
zones <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/HUC12_zones_30m.tif")
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


    # FOR THEOBALD VALLEY BOTTOMS, THERE ARE NO NODATA CELLS, JUST ZEROS; CHECK WITH MEREDITH ABOUT WHAT TO DO HERE


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


# For non-masked area, calculate average stream biotic condition in each HUC
# Multiply proportion by average biotic condition
# Assign condition value to all cells within given HUC



### LAND PROTECTION --> STREAM HEALTH
landpro.streamhealth <- zonal(x=stream.biotic, z=zones, fun="mean", digits=3, na.rm=TRUE)

### LAND PROTECTION --> NORMATIVE FLOW REGIME
landpro.normativeflow <- zonal(x=_____, z=zones, fun="mean", digits=3, na.rm=TRUE)
  
  
### LAND PROTECTION --> WITHIN-HABITAT CONNECTIVITY
landpro.connect <- 

### LAND PROTECTION --> RIPARIAN CONFIGURATION
landpro.riparian <- 


### LAND PROTECTION --> UPLAND VEG COMPOSITION/STRUCTURE
landpro.uplandveg <- 




########################################################################################################################
### BRIDGE/CULVERT UPGRADE #############################################################################################
########################################################################################################################



### BRIDGE/CULVERT UPGRADE --> STREAM HEALTH




### BRIDGE/CULVERT UPGRADE --> WITHIN-HABITAT CONNECTIVITY





########################################################################################################################
### ROAD DECOMMISSIONING ###############################################################################################
########################################################################################################################
# calculate number of road miles eligible for decommissioning in each HUC (or buffered area around roads) as proportion measure
# calculate mean target within buffer as the condition metric

### ROAD DECOMMISSIONING --> STREAM HEALTH

### ROAD DECOMMISSIONING --> WITHIN-HABITAT CONNECTIVITY

### ROAD DECOMMISSIONING --> RIPARIAN CONFIGURATION

### ROAD DECOMMISSIONING --> UPLAND VEG COMPOSITION/STRUCTURE





########################################################################################################################
### GRAZING MANAGEMENT #################################################################################################
########################################################################################################################
# calculate proportion of HUC area available for public-lands grazing as proportion metric
# calculate mean target within public grazing land (and potentially downstream for aquatic targets?) as the condition metric


### GRAZING MANAGEMENT --> STREAM HEALTH

### GRAZING MANAGEMENT --> WITHIN-HABITAT CONNECTIVITY

### GRAZING MANAGEMENT --> RIPARIAN CONFIGURATION

### GRAZING MANAGEMENT --> UPLAND VEG COMPOSITION/STRUCTURE





########################################################################################################################
### IRRIGATION ADJUSTMENT ##############################################################################################
########################################################################################################################
# calculate proportion of HUC that is irrigated as proportion metric
# calculate mean target within buffer (downstream of?) irrigated lands as the condition metric

### IRRIGATION ADJUSTMENT --> STREAM HEALTH

### IRRIGATION ADJUSTMENT --> NORMATIVE FLOW REGIME

### IRRIGATION ADJUSTMENT --> UPLAND VEG COMPOSITION/STRUCTURE

