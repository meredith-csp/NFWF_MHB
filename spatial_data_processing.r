# Spatial data processing for NFWF project

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

# MAIN TASKS

outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/"

### Load shapefiles
huc6 <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/MissouriHeadwaters_HUC6.shp", stringsAsFactors=FALSE)  # Missouri Headwater Basin polygon (HUC6 unit)
huc12 <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/MissouriHeadwaters_HUC12.shp", stringsAsFactors=FALSE)  # HUC12 units within MHB
nhd <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/NHDPlusV21/MHB_streamflow_noZeroOr-9999.shp", stringsAsFactors=FALSE)  # stream network with flow projections from RMRS
prvtcons <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/LandMan_PrvtCons/LandMan_PrvtCons.shp", stringsAsFactors=FALSE)  # privately owned conservation lands (e.g., TNC)
publiclands <- st_read("C:/Work/SpatialData/Montana/MT_PublicLands/MTPublicLands.shp", stringsAsFactors=FALSE)  # public lands in MT
easements <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/MTConEasements_SHP/MTConEasements_SHP.shp", stringsAsFactors=FALSE)  # conservation easements in Montana
ynp <-  st_read("C:/Work/SpatialData/Boundaries/NPS/YNP_Boundary/ynp.shp", stringsAsFactors=FALSE) # Yellowstone National Park polygon (clip this out of MHB)  
blm.graze <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/BLM_Grazing_Allotments/Grazing_Allotments/gra_allot_poly.shp", stringsAsFactors=FALSE)  # BLM grazing allotments in Montana
fs.graze <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/USFS_Grazing_Allotments/R1_Allotments.shp", stringsAsFactors=FALSE)  # Forest Service grazing allotments
dnrc.lands <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/DNRCAgAndGrazing20170928/AgAndGrazing20170928.shp", stringsAsFactors=FALSE)  # MT state trust lands (some have grazing agreements)
vca <-    st_read("C:/Work/SpatialData/NFWF_Cross_Realm/VCA_Region_10u/unconfined_valley_bottom_clipped.shp")  # unconfined valley bottoms
streamflow <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/NHDPlusV21/MHB_streamflow_noZeroOr-9999.shp")
forest.centrality <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/Theobald_flowlines_clipped/Theobald_forest_flowlines.shp")
alpine.centrality <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/Theobald_flowlines_clipped/Theobald_alpine_flowlines.shp")
grassland.centrality <-st_read("C:/Work/SpatialData/NFWF_Cross_Realm/Theobald_flowlines_clipped/Theobald_grassland_flowlines.shp")
shrubland.centrality <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/Theobald_flowlines_clipped/Theobald_shrub_flowlines.shp")
biotic.condition <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/NHDPlusV21/NHDPlusV21_Flowline_MHBclip.shp")
streamtemp <- st_read("C:/Work/SpatialData/NFWF_Cross_Realm/NorWeST_PredictedStreamTempLines_MissouriHW/NorWeST_PredictedStreamTempLines_MissouriHW.shp")

### Load rasters
bps <- raster("C:/Work/SpatialData/NFWF_Cross_Realm/LANDFIRE/US_140BPS_MHBclip.tif")
evt <- raster("C:/Work/SpatialData/NFWF_Cross_Realm/LANDFIRE/US_140EVT_MHBclip.tif")
vdep <- raster("C:/Work/SpatialData/NFWF_Cross_Realm/LANDFIRE/US_140VDEP_MHBclip.tif")
whp <- raster("C:/Work/SpatialData/NFWF_Cross_Realm/WildfireHazardPotential/Data/whp_2014_continuous/whp2014_cnt.tif", stringsAsFactors=FALSE) # wildfire hazard potential (from USFS)
ssurgo <- raster("C:/Work/SpatialData/NFWF_Cross_Realm/soils_GSSURGO_mt_3509679_01/MapunitRaster_mt_10m.tif")
template <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/RasterTemplate/MHB_plu_270.txt") # load raster template from Meredith for Zonation

### Reproject to common projection - use UTM Zone 12, NAD83 (projection used for LANDFIRE data)
newproj <- proj4string(bps)

shapefiles <- c("huc6","huc12","nhd","prvtcons","publiclands","ynp","blm.graze","fs.graze","dnrc.graze")  # names of shapefiles to reproject
for(i in 1:length(shapefiles)) {   # reproject shapefiles
  reproj <- st_transform(get(shapefiles[i]), crs=newproj)
  assign(shapefiles[i], reproj)
}





### Clip input layers to MHB extent -  not sure this is actually necessary, since we're running zonal stats for MHB HUC12s and data outside the HUC polygons will not be considered
croplayernames <- c("nhd","prvtcons","publiclands","easements")  # names of layers you want to crop
mhb.sp <- as(mhb, "Spatial") # convert mhb sf layer to sp (so extent can be extracted by crop function)
for(k in 1:length(croplayernames)) {
  sp.input <- as(get(croplayernames[k]), "Spatial")
  temp1 <- raster::crop(sp.input, mhb.sp, progress = 'text')
  assign(croplayernames[k], temp1)
}
croprastnames <- c("bps","evt")
for(k in 1:length(croprastnames)) {
  sp.input <- get(croprastnames[k])
  temp2 <- raster::crop(sp.input, mhb.sp, progress = 'text')
  assign(croprastnames[k], temp2)
}


### Write cropped, reprojected layers to new folder
outfolder <- "FOLDER PATH HERE/" # path to folder where you want to write these files; NEEDS TO END WITH A FORWARD SLASH
for(l in 1:length(croplayernames)) {
  temp <- as(get(croplayernames[l]),"sf")
  st_write(temp, paste0(outfolder,croplayernames[l],".shp"))
}
for(l in 1:length(croprastnames)) {
  temp <- get(croprastnames[l])
  writeRaster(temp, paste0(outfolder,croprastnames[l],".tif"), format="GTiff", prj=TRUE)
}









# Move all of this below to its own script; will need to add a few lines that load in EVT and BPS rasters



### Reclassify LANDFIRE EVT and BPS layers using common veg types
# load in reclassification matrices (first column is current value, second column is desired value)
bps.rcl.mat <- as.matrix(read.csv("C:/Work/SharedDocs/CLLC/NFWF cross realm/BPS_crosswalk.csv", head=TRUE))[,1:2] 
evt.rcl.mat <- as.matrix(read.csv("C:/Work/SharedDocs/CLLC/NFWF cross realm/EVT_crosswalk.csv", head=TRUE))[,1:2]
# reclassify
bps.rcl <- reclassify(bps, bps.rcl.mat)
evt.rcl <- reclassify(evt, evt.rcl.mat)
# create transition raster
trans.rcl <- bps.rcl    # make a new raster with NA values but same extent/resolution as evt and bps
trans.rcl[,] <- NA
trans.mat <- matrix(1:36, nrow=6, ncol=6, byrow=TRUE)  # transition matrix showing new value for each combination of BPS and EVT value
for(i in 1:6){
  for(j in 1:6){
    trans.rcl[bps.rcl==i & evt.rcl==j] <- trans.mat[i,j]
  }
}

# create separate raster layers for each cover type (set all other cells NA)
evt.developed <- evt.rcl
evt.developed[evt.developed != 1] <- NA
evt.forest <- evt.rcl
evt.forest[evt.forest != 2] <- NA
evt.grassland <- evt.rcl
evt.grassland[evt.grassland != 3] <- NA
evt.othernat <- evt.rcl
evt.othernat[evt.othernat != 4] <- NA
evt.riparian <- evt.rcl
evt.riparian[evt.riparian != 5] <- NA
evt.shrubland <- evt.rcl
evt.shrubland[evt.shrubland != 6] <- NA

# write reclassified raster layers to spatial data folder
writeRaster(bps.rcl, paste0(outfolder, "BPS_reclass.tif"), format="GTiff", prj=TRUE)
writeRaster(evt.rcl, paste0(outfolder, "EVT_reclass.tif"), format="GTiff", prj=TRUE)
writeRaster(trans.rcl, paste0(outfolder, "veg_transition.tif"), format="GTiff", prj=TRUE)
writeRaster(evt.developed, paste0(outfolder, "EVT_developed.tif"), format="GTiff", prj=TRUE)
writeRaster(evt.forest, paste0(outfolder, "EVT_forest.tif"), format="GTiff", prj=TRUE)
writeRaster(evt.grassland, paste0(outfolder, "EVT_grassland.tif"), format="GTiff", prj=TRUE)
writeRaster(evt.othernat, paste0(outfolder, "EVT_otherNatural.tif"), format="GTiff", prj=TRUE)
writeRaster(evt.riparian, paste0(outfolder, "EVT_riparian.tif"), format="GTiff", prj=TRUE)
writeRaster(evt.shrubland, paste0(outfolder, "EVT_shrubland.tif"), format="GTiff", prj=TRUE)

# Generate specific transition layers (pull out values for relevant transitions)
constant <- c(1,8,15,22,29,36)  # BPS=EVT
riparian.loss <- c(25,26,27,28,30)   # BPS=riparian, EVT=not riparian
forest.encroach <- c(14,20,26,32)   # BPS=Grassland/shrubland/OtherNatural/Riparian, EVT=Forest
shrub.encroach <- c(12,18,24)  # BPS=forest/grassland/otherNatural, EVT=shrubland

'%notin%' <- function(x,y) !(x %in% y)
evt.forest.encroach <- trans.rcl
evt.forest.encroach[evt.forest.encroach %notin% forest.encroach] <- NA

### Generate mask layers

# Easements: mask includes public lands, existing easements, private lands managed for conservation
# Thinning: mask out non-forest and areas with low fuel loads
# Prescribed fire: mask out areas with low fuel loads
# Invasive species removal: ? 
# Riparian exclusion: mask out non-riparian areas (based on Theobald floodplain mapping)
# Beaver mimicry: talk to Nathan Korb first
# Riparian revegetation: mask out non-riparian areas and riparian that still has native vegetation
# Crossing structures: no mask needed? Just extract connectivity values along road network?