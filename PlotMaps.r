############
#---TODO---#
############
# Make it work in two modes: partial data, full globe
# Do sth about bad text labelling (centers)
# Add more options (font size, vertical, horizontal adjust)
# Supress all the warnings from rgdal::readOGR 
# parse the data and labels of the locations from kml file only
# add some jitter to labels positions to avoid overplotting

###########
#---RUN---#
###########
#adjust path here:
#setwd("C:\\Documents and Settings\\filip\\My Documents\\My Dropbox\\Phyleography\\PlotMaps")
setwd("/home/filip/Dropbox/Phyleography/PlotMaps")

source("PlotMaps_libs.r")
source("PlotMaps_ggplotUtils.r")
source("PlotMaps_GUI.r")



