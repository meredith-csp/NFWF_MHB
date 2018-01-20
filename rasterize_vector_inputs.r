# SCRIPT TO CONVERT VECTOR INPUT LAYERS TO RASTER AND RESAMPLE TO MATCH HUC12 TEMPLATE

library(raster)
library(fasterize)
library(sf)
library(sp)
library(parallel)

outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/"  # folder for rasterized outputs
template30 <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Rasterized Inputs/MHB_huc12_zones_30m.tif")  # template to which new rasters should be snapped



###############################################################################################################################
# For line or point layers (must use rasterize)

### USER INPUTS
infile <- "C:/Work/SpatialData/NFWF_Cross_Realm/Theobald_flowlines_clipped/Theobald_shrub_flowlines.shp"  # name of shapefile to convert
fieldname <- "GRID_CODE"  # name of the field in the shapefile that you want to use to assign raster value; could also just use 1 for constant value
outfilename <- "shrubland_centrality_flowlines.tif"   # what you want to call the output file
functionname <- "max" # name of the function to deal with with overlapping features (min, max, mean, first, last, or count)
###

# Normal version:
vector.sf <- st_read(infile)   # read in shapefile
# if dimension is XYZM:
#vector.sf <- st_zm(vector.sf, drop=TRUE, what="ZM")
vector.sf <- st_transform(vector.sf, proj4string(template30))# reproject shapefile to match template 
vector.sp <- as(vector.sf, "Spatial")  # convert to sp object to allow rasterizing
outraster <- rasterize(x=vector.sp, y=template30, field=fieldname, fun=functionname, na.rm=TRUE, progress="window")
writeRaster(outraster, paste0(outfolder, outfilename), format="GTiff", prj=TRUE)

# Parallelized version:
vector.sf <- st_read(infile)   # read in shapefile
# if dimension is XYZM:
#vector.sf <- st_zm(vector.sf, drop=TRUE, what="ZM")
vector.sf <- st_transform(vector.sf, proj4string(template30))# reproject shapefile to match template 
vector.sp <- as(vector.sf, "Spatial")  # convert to sp object to allow rasterizing
no_cores <- detectCores() - 1  # Calculate the number of cores
features <- 1:nrow(vector.sp)  # Number of features in vector layer
parts <- split(features, cut(features, no_cores))
cl <- makeCluster(no_cores, type = "PSOCK")  # Initiate cluster (after loading all the necessary object to R environment: BRA_adm2, parts, r.raster, n)
print(cl)
clusterExport(cl, list("rasterize", "vector.sp", "parts", "template30", "fieldname"))
rParts <- parLapply(cl = cl, X = features, fun = function(x) rasterize(vector.sp[parts[[x]],], template30, fieldname, na.rm=TRUE, progress="window")) # Parallelize rasterize function
stopCluster(cl) # Finish
outraster <- do.call(merge, rParts)  # Merge all raster parts
plot(outraster) # Plot raster















###############################################################################################################################
# For polygon layers (can use fasterize, which is several orders of magnitude faster and works with sf inputs)

### USER INPUTS
infile <- "C:/Work/SpatialData/NFWF_Cross_Realm/Mask layers/conservation_easement_mask.shp"  # name of shapefile to convert
fieldname <- NULL  # name of the field in the shapefile that you want to use to assign raster value; if NULL, all polygons given value of 1
outfilename <- "conservation_easement_mask.tif"   # what you want to call the output file
functionname <- "last" # name of the function to deal with with overlapping features ("sum", "first", "last", "min", "max", "count", or "any")
###

vector.sf <- st_read(infile)   # read in shapefile
vector.sf <- st_transform(vector.sf, proj4string(template30))# reproject shapefile to match template 
outraster <- fasterize(sf=vector.sf, raster=template30, field=fieldname, fun=functionname, progress="window")
writeRaster(outraster, paste0(outfolder, outfilename), format="GTiff", prj=TRUE)

