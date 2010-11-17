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
    svalue(status_bar) <- "Saving to PNG..."
    gfile(text = "Save as png...", type = "save", initialfilename = "Rplot.png", 
        handler = get.png, filter = list(`All files` = list(patterns = c("*")), 
            `png files` = list(patterns = c("*.png"))))
}

menulist = list(
    SaveAsEps = gbutton("Save to EPS", handler = SavePlot2Eps),
    SaveAsPng = gbutton("Save to PNG", handler = SavePlot2Png),
    help = gbutton("Help", handler = HelpHandler),
    separator=gseparator(),
    quit = gbutton("Quit", handler = function(h, ...) {
    dispose(window)
	})
  )

window <- gwindow("PlotMap Widget")
Menu <- gtoolbar(menulist, cont = TRUE, container = window, style="both")

## Push buttons to right
#addSpring(menulist)
BigGroup <- ggroup(cont = window)
group <- ggroup(horizontal = FALSE, container = BigGroup)
add(BigGroup, ggraphics())

gseparator(container = group)

#####---TEMPORARY---#########################################
tmp <- gframe("Path to locations file", container = group)
LocFile = gedit("/home/filip/Dropbox/Phyleography/PlotMaps/supplementary/locationHIV2A.txt", 
    width = 15, container = tmp)

tmp <- gframe("Path to bayes factors file", container = group)
OutFile = gedit("/home/filip/Dropbox/Phyleography/PlotMaps/supplementary/WA_HIV2A.out", 
    width = 15, container = tmp)
######---END TEMPORARY---###################################

tmp <- gframe("min/max longitude", container = group)
MinLon = gedit("-17.0", coerce.with = as.numeric, 
    width = 5, container = tmp)
MaxLon = gedit("7.0", coerce.with = as.numeric, width = 5, 
    container = tmp)

tmp <- gframe("min/max latitude", container = group)
MinLat = gedit("4.0", coerce.with = as.numeric, width = 5, 
    container = tmp)
MaxLat = gedit("15.5", coerce.with = as.numeric, width = 5, 
    container = tmp)

#coordinate system selection from drop-down list
tmp <- gframe("Select coordinates", container = group)
SelectCoord <- gdroplist(c("map", "cartesian"), container = tmp)

#slider to choose size of locations points
tmp <- gframe("Choose points size", container = group)
locations_size <- gspinbutton(from = 0, to = 10, by = 1, value = 4, container = tmp)

#droplist for poly_color ( play with them and choose some nice ones )
tmp <- gframe("Polygons color", container = group)
poly_color <- gdroplist( c("burlywood", "grey20" ), container = tmp, editable = TRUE)

#droplist for boundaries_color ( play with them and choose some nice ones )
tmp <- gframe("Polygon boundaries color", container = group)
boundaries_color <- gdroplist( c("grey20", "grey60" ), container = tmp, editable = T)

#droplist for text_labels_col ( play with them and choose some nice ones )
tmp <- gframe("Polygon boundaries color", container = group)
text_labels_col <- gdroplist( c("black", "orange" ), container = tmp, editable = T)

#plot button
tmp <- gframe("Plot data on map", container = group)
add(tmp, gbutton("plot", handler = PlotOnMap))

#status bar
status_bar <- gstatusbar("Welcome. Expand widget for full screen view.", 
    container = window)

# Color choice
#c("burlywood", "grey20", "black", "orange", "grey60")





