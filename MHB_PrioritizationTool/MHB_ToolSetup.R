##########################################################################
#                                                                        #
#  Missouri Headwaters Basin Cross-Realm Management Prioritization Tool  #
#                           ONE-TIME SETUP                               #
#                                                                        #
#  The command below will install the R packages necessary to run the    #
#  MHB prioritization tool on your computer. To run the install, simply  #
#  click the 'Source' button on the right side of the header above.      #
#                                                                        #
#  This install process should take about 10 minutes, and you'll only    #
#  need to run it before your first use of the tool.                     #
#                                                                        #
##########################################################################

install.packages(
  c("rmarkdown","flexdashboard","raster","rgdal","rgeos","leaflet","mapview","RColorBrewer","zonator"),
  repos = "http://cran.us.r-project.org")