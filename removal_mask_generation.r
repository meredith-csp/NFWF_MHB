### Generate removal masks for each action
# Pixels available to implementing action have a value of 1; pixel unavailable have a value of 0

require(raster)

infolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs NAD83 UTM/"  # folder holding prepped input rasters (should have same projection, resolution, and origin as HUC12 zones layer)
outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Removal Masks NAD83 UTM/"
zones <- raster(paste0(infolder, "HUC12_zones_30m_NAD83UTM.tif"))  # zones template



### Grazing management - mask includes all public grazing allotments, as well as lands classified as "Pasture/hay" in NLCD
public.grazing <- raster(paste0(infolder, "public_grazing_lands_NAD83UTM.tif"))
public.grazing[is.na(public.grazing)==TRUE] <- 0
pasture <- raster(paste0(infolder,"nlcd_2011_NAD83UTM_v3.tif"))   # read in NLCD layer
pasture[pasture!=81] <- 0 # set everything that is not pasture to 0
pasture[pasture==81] <- 1   # set pasture to 1
graze.stack <- stack(public.grazing, pasture)
all.grazing <- raster::calc(graze.stack, function(x) {max(x)})
writeRaster(all.grazing, paste0(outfolder,"grazing_mgmt_removal_mask.tif"))  # write output



### Stream/riparian restoration - mask includes undeveloped valley bottoms that are not waterbodies
valley <- raster(paste0(infolder, "Theobald_valley_bottom_30m_NAD83UTM.tif"))  # read in 30-m valley bottom layer (use 30-m for mask, but 10-m for calculating condition metrics)
valley[valley>0] <- 1  # convert from continuous scale to valley (=1) or non-valley (=0)
valley[is.na(valley)==TRUE] <- 0
undeveloped <- raster(paste0(infolder,"nlcd_2011_NAD83UTM_v3.tif"))   # read in NLCD layer
developed.ids <-  c(21,22,23,24)
undeveloped[!undeveloped %in% developed.ids] <- 1
undeveloped[undeveloped %in% developed.ids] <- 0
waterbody <- raster(paste0(infolder,"lakes_ponds_reservoirs_NAD83UTM.tif"))
waterbody[waterbody==1] <- 0
waterbody[is.na(waterbody)==TRUE] <- 1
undev.valley <- undeveloped * valley * waterbody
writeRaster(undev.valley, paste0(outfolder,"stream_riparian_restoration_removal_mask_NAD83UTM.tif"))  # write output



### Land protection - mask includes private lands that are not already under conservation management
unprotected.land <- raster(paste0(infolder, "conservation_easement_mask_NAD83UTM.tif"))  
unprotected.land[is.na(unprotected.land)==TRUE] <- 0
writeRaster(unprotected.land, paste0(outfolder,"land_protection_removal_mask_NAD83UTM.tif"))  # write output



### Bridge/culvert upgrade - mask includes cells with a bridge in the National Bridge Inventory (with or without a buffer to increase visibility in map output)
# no buffer
bridges.nobuffer <- raster(paste0(infolder, "bridges_NAD83UTM.tif")) 
bridges.nobuffer[is.na(bridges.nobuffer)==TRUE] <- 0
writeRaster(bridges.nobuffer, paste0(outfolder,"bridges_no_buffer_removal_mask_NAD83UTM.tif"))  # write output
# 1-km buffer
bridges.1kmbuffer <- raster(paste0(infolder, "bridges_1km_buffer_NAD83UTM.tif"))
bridges.1kmbuffer[is.na(bridges.1kmbuffer)==TRUE] <- 0
writeRaster(bridges.1kmbuffer, paste0(outfolder,"bridges_1km_buffer_removal_mask_NAD83UTM.tif"))  # write output



### Road decommissioning - mask includes decommissionable road types on public lands
roads <- raster(paste0(infolder, "TIGER_roads_mtfcc_NAD83UTM.tif")) # read in TIGER roads layer
decom.ids <- c(4,5,9)  # values for types of roads that are decommissionable (4 = S1500/vehicular trail, 6 = S1640/service drive, 9 = S1740/service road for private vehicles)
roads[!roads %in% decom.ids] <- 0
roads[roads %in% decom.ids] <- 1
private <- raster(paste0(infolder, "private_land_NAD83UTM.tif"))  # read in private lands layer
private[is.na(private)==FALSE] <- 0   # set private lands to have a value of zero
private[is.na(private)==TRUE] <- 1  # set public lands to have a value of one
public.roads <- private * roads   # multiply together so pixels that are public and roads get value of one, all others get value of zero
writeRaster(public.roads, paste0(outfolder,"road_decommissioning_removal_mask_NAD83UTM.tif"))  # write output



### Irrigation adjustment - mask in cludes all irrigated lands
irrig <- raster(paste0(infolder, "irrigated_land_NAD83UTM.tif"))  # read in irrigated lands raster
irrig[is.na(irrig)==TRUE] <- 0 
writeRaster(irrig, paste0(outfolder,"irrigation_adjustment_removal_mask_NAD83UTM.tif"))  # write output



### Soil health management - mask includes all crop land
crops <- raster(paste0(infolder,"nlcd_2011_NAD83UTM_v3.tif"))   # read in NLCD layer
crops[crops!=82] <- 0   # set everything that is not crops to 0
crops[crops==82] <- 1   # set crops to 1
writeRaster(crops, paste0(outfolder,"soil_mgmt_removal_mask_NAD83UTM.tif"), overwrite=TRUE)  # write output



### Prescribed fire/thinning - mask includes all forest, shrub, and grassland types (excluding those in residential areas)
veg.to.manage <- raster(paste0(infolder,"nlcd_2011_NAD83UTM_v3.tif"))
forest.ids <- c(41,42,43)
shrub.ids <- c(51,52)
grass.ids <- 71
good.ids <- c(forest.ids,grass.ids,shrub.ids)
veg.to.manage[!veg.to.manage %in% good.ids] <- 0 # set everything that is not forest, shrub, or grass to 0
veg.to.manage[veg.to.manage %in% good.ids] <- 1 # set forest, shrub, or grass to 1
writeRaster(veg.to.manage, paste0(outfolder,"prescribed_fire_thinning_removal_mask_NAD83UTM.tif"))  # write output


