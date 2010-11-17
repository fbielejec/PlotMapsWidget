############
#---TODO---#
############
# Make it work in two modes: partial data, full globe
# Add more options (font size, vertical, horizontal adjust, colors, bins...)
# Supress all the warnings from rgdal::readOGR 
# add some jitter to labels positions to avoid overplotting
# add more tryCatch blocks, especially for plotting

###########
#---RUN---#
###########
#adjust path here:
#setwd("C:\\Documents and Settings\\filip\\My Documents\\My Dropbox\\Phyleography\\PlotMaps")
setwd("/home/filip/Dropbox/Phyleography/PlotMaps")

source("PlotMaps_libs.r")
source("PlotMaps_ggplotUtils.r")
source("PlotMaps_GUI.r")



