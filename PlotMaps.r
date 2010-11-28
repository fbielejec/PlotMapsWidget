############
#---TEST---#
############
# /home/filip/Dropbox/Phyleography/PlotMaps/supplementary/Test/indicators.log
# /home/filip/Dropbox/Phyleography/PlotMaps/supplementary/Test/locationDegrees
#
#################
##---Philippe---#
#################
# /home/filip/Dropbox/Phyleography/PlotMaps/supplementary/Philippe/genomes.HKYG.UCLN.EGC.DISC.BSSVS.Indicator.log
# /home/filip/Dropbox/Phyleography/PlotMaps/supplementary/Philippe/locationDegrees
#
##############
##---Nuno---#
##############
# /home/filip/Dropbox/Phyleography/PlotMaps/supplementary/Nuno/HIV2A_WAcombi_equalfreq_bssvs_rateMatrix.log
# /home/filip/Dropbox/Phyleography/PlotMaps/supplementary/Nuno/locationHIV2A.txt
#
###########
#---RUN---#
###########
#adjust path here:
#setwd("C:\\Documents and Settings\\filip\\My Documents\\My Dropbox\\Phyleography\\PlotMaps")
setwd("/home/filip/Dropbox/Phyleography/PlotMaps")

source("PlotMaps_libs.r")
source("Combinatorics.R")
source("RateIndicatorBF.R")
source("PlotMaps_ggplotUtils.r")
source("GetKML.R")
source("PlotMaps_GUI.r")