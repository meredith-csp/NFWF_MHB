## Zonal statistics to generate condition layers for Zonation
# Note that this is for raster inputs only - will need separate script for vector inputs (or will have to convert to raster first)

library(raster)

######################################################################################################################
# USER INPUTS
zones <- raster("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/RasterTemplate/mhb_plu_270.tif")  # HUC12 raster (i.e., the "zones" in zonal statistics")
infolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Test Layers/"  # folder holding prepped input rasters (should have same projection, resolution, and origin as HUC12 zones layer)
outfolder <- "C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data/Condition Layers/"   # folder where condition raster will be written
inputname <- "test_forest_connectivity.tif"   # name of layer for which you want to calculate zonal stats
outputname <- "max_forest_connectivity.tif"  # name you want to assign output
statistic <- "max"  # name of statistic you want to calculate for each zone
                    # options are 'mean', 'sd', 'min', 'max', 'sum'; or, for relatively small Raster* objects, a proper function
nodataval <- 0   # what value should be assigned for zonal statistic when HUC12 unit does not contain any data (e.g., unit that doesn't contain any connectivity flowlines)
relationship <- "positive"  # for rescaling from 0 to 1, should higher zonal statistic values represent greater opportunity in Zonation ("positive") or less opportunity ("negative")?
######################################################################################################################

  
inputdata <- raster(paste0(infolder, inputname))  # load input data
zonal.mat <- zonal(x=inputdata, z=zones, fun="max", digits=0, na.rm=TRUE)  # calculate zonal statistics
zonal.mat[zonal.mat=="-Inf"] <- nodataval  # replace -Inf values with zeros
original <- zonal.mat[,2]
if(relationship=="positive"){   # rescale from 0 (low opportunity) to 1 ((high opportunity)
  rescaled <- (original - min(original))/(max(original)-min(original))
}
if(relationship=="negative"){
  rescaled <- 1 - (original - min(original))/(max(original)-min(original))
}
reclass.mat <- as.matrix(cbind(zonal.mat[,1],rescaled))  # create reclassification table
condition <- reclassify(zones, zonal.mat) # reclassify using zonal.mat (replace zone ID with zonal stat value for each cell)
writeRaster(condition, paste0(outfolder, outputname), format="GTiff", prj=TRUE)  # write output raster to Drive




