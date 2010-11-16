###########
#---GUI---#
###########
HelpHandler <- function(h,...) {
gmessage(title = "About PlotMaps Widget", "TODO\n Don't touch my stuff. It's booby trapped!")
}

KmlOpenerHandler <- function(h, ...) {

  svalue(status_bar) <- "Uploading data...";
  
  gfile(
    text    = "Upload KML file",
    type    = "open",
    handler = function(h, ...)
    {
      tryCatch(
        {
          the_data <- do.call("getKmlData", list(h$file) )
          assign("dataset", the_data[[1]], envir = globalenv() )
          assign("places", the_data[[2]], envir = globalenv() )
          svalue(status_bar) <- "Data from kml file uploaded"
        },
        error = function(e) svalue(status_bar) <- "Could not upload data from kml file"
      )
    },
    filter = list(
      "KML files" = list(patterns = c("*.kml")),
      "All files" = list(patterns = c("*"))
    )
  )
}



get.eps=function(h,...) {
local({
dev.set (2)
ggsave(h$file)
#dev.print(device=postscript, file=h$file, onefile=FALSE, paper="special", horizontal=FALSE);
})
svalue(status_bar) <- "Done"
}

get.png <- function(h,...) {
local({
dev.set (2)
ggsave(h$file)
#dev.print(png, file=h$file, width=2048, height=1024)
})
svalue(status_bar) <- "Done"
}

SavePlot2Eps <- function(h,...){
svalue(status_bar) <- "Saving to EPS..."
   gfile(text="Save as eps...", type="save", initialfilename = "Rplot.eps", handler = get.eps, filter = 
   list(
   "All files" = list(patterns = c("*")), 
   "eps files" = list(patterns = c("*.eps")) )
    )
}

SavePlot2Png <- function(h,...){
svalue(status_bar) <- "Saving as PNG"
   gfile(text="Save as png...", type="save", initialfilename = "Rplot.png", handler = get.png, filter = 
   list(
   "All files" = list(patterns = c("*")), 
   "png files" = list(patterns = c("*.png")) )
    )
}


  window <- gwindow("PlotMap Widget")
  BigGroup <- ggroup(cont = window)
  group <- ggroup(horizontal=FALSE, container = BigGroup)
  add(BigGroup, ggraphics())

  tmp <- gframe("Specify KML layer", container = group)
KmlLayer =  gedit("discrete rates with bayes factor larger than 3.0", width=15, container = tmp) 
 
  tmp <- gframe("Open KML file", container = group)
KMlOpener = gbutton("Open", handler =  KmlOpenerHandler, container=tmp )
 
tmp <- gframe("min/max longitude", container = group )
MinLon = gedit("-20.0", coerce.with=as.numeric, width=5, container = tmp) 
MaxLon = gedit("10.5", coerce.with=as.numeric, width=5, container = tmp) 

tmp <- gframe("min/max latitude", container = group )
MinLat = gedit("2.0", coerce.with=as.numeric, width=5, container = tmp)
MaxLat = gedit("16.5", coerce.with=as.numeric, width=5, container = tmp) 

#coordinate system selection from drop-down list
  tmp <- gframe("Select coordinates", container = group)
SelectCoord <- gdroplist(c("map", "cartesian"), container = tmp)

  tmp <- gframe("Plot data on map", container = group)
 add(tmp, gbutton("plot", handler =  PlotOnMap))
  
   tmp <- gframe("Save as EPS", container = group)
 add(tmp, gbutton("Save", handler =  SavePlot2Eps))
tmp <- gframe("Save as PNG", container = group)
 add(tmp, gbutton("Save", handler =  SavePlot2Png))
 
gseparator(container = group)

gbutton("Help", handler = HelpHandler, container = group )

gbutton("Quit", handler =  function(h,...) {
	dispose(window)
	}, 
	container = group )

status_bar <- gstatusbar("Welcome. Expand widget for full screen view.", container = window)

