#Setting up Zonation run for Missouri Headwaters

###
#NAME THE ZONATION RUN
rname<- "01_mhb_test"

###
#Create required target features file specifying feature rasters and parameters

#ENTER CONSERVATION TARGET RASTER FILENAMES (ULTIMATELY WILL WANT USERS TO CHECK BOXES FOR WHICH TO INCLUDE):
targ.files<-c("a.tif","b.tif","c.tif")

#ENTER WEIGHTS TO BE ASSIGNED TO EACH TARGET (DEFAULT TO EQUAL WEIGHTS)
targ.wts<-c(1.0, 1.0, 1.0)

#Set connectivity weights to dummy -1 (unless reason to offer as option in this context?)
#Set loss sensitivity/target-based planning rep column to generic 0.25
targ.conn<-matrix(-1,nrow=length(targ.files),ncol=3)
targ.othr<-rep(0.25,length(targ.files))

prj.spp<-cbind(targ.wts,targ.conn,targ.othr,targ.files)
row.names(prj.spp)<-NULL; colnames(prj.spp)<-NULL

feat.spp<-paste("input/",rname,".spp",sep="")
fileConn<-file(feat.spp)
write.table(prj.spp, fileConn)
close(fileConn)

###
#Create required settings file specifying run parameters

#SELECT CELL REMOVAL RULE (ULTIMATELY WILL WANT USERS TO CHECK BOX FOR SELECTION, OR DEFAULT TO BEST FOR MHB CASE)
#1=Basic Core-Area Zonation, 2=Additive Benefit Function, 3=Target-Based Planning, 4=Generalized Benefit Function, 5=Random Removal
rem.rule<-2

#SELECT WARP FACTOR (How many cells removed at a time. Lower = finer solution, longer run-time. Default=100)
warp.fac<-100

#SELECT CELL REMOVAL METHOD (1=Remove frome edges of remaining landscape, 0=Remove from anywhere in landscape)
edge.rem<-1

#CHOOSE WHETHER TO USE PLANNING UNIT LAYER; IF SO, IDENTIFY FILEPATH 
plan.ly<-0
plan.nm<-""

#CHOOSE WHETHER TO USE COST LAYER; IF SO, IDENTIFY FILEPATH
cost.ly<-0
cost.nm<-""

#CHOOSE WHETHER TO USE REMOVAL MASK LAYER; IF SO, IDENTIFY FILEPATH
mask.ly<-0
mask.nm<-""

#CHOOSE WHETHER TO USE DIRECTED CONNECTIVITY (DEFAULT TO  YES); IDENTIFY FILEPATH
tree.ly<-1
tree.nm<-"mhb_dirconn.tif"  #File format?

#CHOOSE WHETHER TO ACCOUNT FOR ADMINISTRATIVE UNITS IN ANALYSIS; IF SO, IDENTIFY FILEPATHS FOR LAYER AND WEIGHTS
admu.ly<-0
admu.nm<-""
admu.wt<-""  #How are these defined? Predefine for each major admin unit type in MHB? File format?

txt.lines<-c("[Settings]",
             paste("removal rule =",rem.rule),
             paste("warp factor =",warp.fac),
             paste("edge removal =",edge.rem),
             paste("use planning unit layer =",plan.ly), paste("planning unit layer file =",plan.nm),
             paste("use cost =",cost.ly), paste("cost file =",cost.nm),
             paste("use mask =",mask.ly), paste("mask file =",mask.nm),
             paste("use tree connectivity =",tree.ly), paste("tree connectivity file =",tree.nm),
             "[Administrative units]",
             paste("use ADMUs =",admu.ly), 
             paste("ADMU layer file =",admu.nm), paste("ADMU descriptions file =",admu.wt))
txt.lines<-matrix(txt.lines,length(txt.lines),1)

set.dat<-paste("input/",rname,".dat",sep="")
fileConn<-file(set.dat)
write.table(txt.lines, fileConn)
close(fileConn)

###
#Create required project file specifying file paths

#ENTER OUTPUT FILE PATH:
  out.txt<-paste("output/",rname,".txt"
  
prj.txt<- paste("call zip4.exe -r",set.dat, feat.spp, out.txt, "0.0 0 1.0 0")
fileConn<-file(paste("input/",rname,".bat",sep=""))
writeLines(prj.txt, fileConn)
close(fileConn)

###
#Run Zonation

