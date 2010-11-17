###########
#---GUI---#
###########
HelpHandler <- function(h, ...) {
    gmessage(title = "About PlotMaps Widget", "TODO\n Don't touch my stuff. It's booby trapped!")
}


get.eps = function(h, ...) {
    local({
        dev.set(2)
        ggsave(h$file)
        #dev.print(device=postscript, file=h$file, onefile=FALSE, paper='special', horizontal=FALSE);
    })
    svalue(status_bar) <- "Done"
}

get.png <- function(h, ...) {
    local({
        dev.set(2)
        ggsave(h$file)
        #dev.print(png, file=h$file, width=2048, height=1024)
    })
    svalue(status_bar) <- "Done"
}

SavePlot2Eps <- function(h, ...) {
    svalue(status_bar) <- "Saving to EPS..."
    gfile(text = "Save as eps...", type = "save", initialfilename = "Rplot.eps", 
        handler = get.eps, filter = list(`All files` = list(patterns = c("*")), 
            `eps files` = list(patterns = c("*.eps"))))
}

SavePlot2Png <- function(h, ...) {
    svalue(status_bar) <- "Saving as PNG"
    gfile(text = "Save as png...", type = "save", initialfilename = "Rplot.png", 
        handler = get.png, filter = list(`All files` = list(patterns = c("*")), 
            `png files` = list(patterns = c("*.png"))))
}


window <- gwindow("PlotMap Widget")
BigGroup <- ggroup(cont = window)
group <- ggroup(horizontal = FALSE, container = BigGroup)
add(BigGroup, ggraphics())

#####---TEMPORARY---#########################################
tmp <- gframe("Path to locations file", container = group)
LocFile = gedit("/home/filip/Dropbox/Phyleography/PlotMaps/supplementary/locationHIV2A.txt", 
    width = 15, container = tmp)

tmp <- gframe("Path to bayes factors file", container = group)
OutFile = gedit("/home/filip/Dropbox/Phyleography/PlotMaps/supplementary/WA_HIV2A.out", 
    width = 15, container = tmp)
######---END TEMPORARY---###################################

tmp <- gframe("min/max longitude", container = group)
MinLon = gedit("-20.0", coerce.with = as.numeric, 
    width = 5, container = tmp)
MaxLon = gedit("10.5", coerce.with = as.numeric, width = 5, 
    container = tmp)

tmp <- gframe("min/max latitude", container = group)
MinLat = gedit("2.0", coerce.with = as.numeric, width = 5, 
    container = tmp)
MaxLat = gedit("16.5", coerce.with = as.numeric, width = 5, 
    container = tmp)

#coordinate system selection from drop-down list
tmp <- gframe("Select coordinates", container = group)
SelectCoord <- gdroplist(c("map", "cartesian"), container = tmp)

tmp <- gframe("Plot data on map", container = group)
add(tmp, gbutton("plot", handler = PlotOnMap))

tmp <- gframe("Save as EPS", container = group)
add(tmp, gbutton("Save", handler = SavePlot2Eps))
tmp <- gframe("Save as PNG", container = group)
add(tmp, gbutton("Save", handler = SavePlot2Png))

gseparator(container = group)

gbutton("Help", handler = HelpHandler, container = group)

gbutton("Quit", handler = function(h, ...) {
    dispose(window)
}, container = group)

status_bar <- gstatusbar("Welcome. Expand widget for full screen view.", 
    container = window)

