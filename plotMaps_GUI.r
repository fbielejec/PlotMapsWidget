###########
#---GUI---#
###########
HelpHandler <- function(h, ...) {
    gmessage(title = "About PlotMaps Widget", "TODO\n Don't touch my stuff. It's booby trapped!")
}


GetEps <- function(h, ...) {		
	tryCatch({
				local({
        dev.set(2)
        ggsave(h$file)
        #dev.print(device=postscript, file=h$file, onefile=FALSE, paper='special', horizontal=FALSE);
    })
svalue(status_bar) <- "Done."
	}, error = function(e) svalue(status_bar) <- "Could not save!")
}


GetPng <- function(h, ...) {	
	tryCatch({
				local({
        dev.set(2)
        ggsave(h$file)
        #dev.print(png, file=h$file, width=2048, height=1024)
    })
svalue(status_bar) <- "Done."
	}, error = function(e) svalue(status_bar) <- "Could not save!")
}


GetCsv <- function(h, ...) {
	tryCatch({  	
				write.table(out, file = h$file, sep = " ", row.names = F, col.names = T)
				svalue(status_bar) <- "Done."			
			}, error = function(e) svalue(status_bar) <- "Could not save!")
}


SavePlot2Eps <- function(h, ...) {
    svalue(status_bar) <- "Saving to EPS..."
    gfile(text = "Save as eps...", type = "save", initialfilename = "Rplot.eps", 
        handler = GetEps, filter = list(`All files` = list(patterns = c("*")), 
            `eps files` = list(patterns = c("*.eps"))))
}


SavePlot2Png <- function(h, ...) {
    svalue(status_bar) <- "Saving to PNG..."
    gfile(text = "Save as png...", type = "save", initialfilename = "Rplot.png", 
        handler = GetPng, filter = list(`All files` = list(patterns = c("*")), 
            `png files` = list(patterns = c("*.png"))))
}

Save2Csv <- function(h, ...) {
	svalue(status_bar) <- "Saving to CSV..."
	gfile(text = "Save as csv...", type = "save", initialfilename = "BFtest.out", 
			handler = GetCsv, filter = list(`All files` = list(patterns = c("*")), 
					`csv files` = list(patterns = c("*.out", "*.csv"))))
}

Save2KML <- function(h, ...) {
	svalue(status_bar) <- "Saving to KML..."
	gfile(text = "Save as csv...", type = "save", initialfilename = "output.kml", 
			handler = GetKML, filter = list(`All files` = list(patterns = c("*")), 
					`csv files` = list(patterns = c("*.kml"))))
}

# not yet implemented:
ExitGracefully <- function(h,...) {
	svalue(status_bar) <- "TODO"
}


menulist <- list(
	cancel = gbutton("cancel", handler = ExitGracefully),
	save_as_kml = gbutton("Save to KML", handler = Save2KML),
    save_as_eps = gbutton("Save to EPS", handler = SavePlot2Eps),
    save_as_png = gbutton("Save to PNG", handler = SavePlot2Png),
	save_as_csv = gbutton("Save to CSV", handler = Save2Csv),
    help = gbutton("Help", handler = HelpHandler),
    separator=gseparator(),
    quit = gbutton("Quit", handler = function(h, ...) {
    dispose(window)
	})
  )

  
window <- gwindow("PlotMaps Widget")
menu  <- gtoolbar(menulist, cont = TRUE, container = window, style = "both")

big_group <- ggroup(cont = window)
group <- ggroup(horizontal = FALSE, container = big_group)
add(big_group, ggraphics())

tmp <- gframe("Path to log file", container = group)
log_file = gedit("/home/filip/Dropbox/GPUProject/Bahl_data/Bahl.noTime.BSSVS.epoch1.log", 
		width = 25, container = tmp)

tmp <- gframe("Path to locations file", container = group)
loc_file = gedit("/home/filip/Dropbox/GPUProject/Bahl_data/locationDegrees.txt", 
    width = 25, container = tmp)

# coordinate system selection from drop-down list
tmp <- gframe("Arrow tips / Coordinates", container = group)
select_arrow_tips <- gdroplist(c("last", "first", "both", "none"), container = tmp)
select_coord      <- gdroplist(c("map", "cartesian", "full globe"), container = tmp)

tmp <- gframe("min/max longitude", container = group)
min_lon_select = gedit("-17.0", coerce.with = as.numeric, width = 5, container = tmp)
max_lon_select = gedit("7.0", coerce.with = as.numeric, width = 5, container = tmp)

tmp <- gframe("min/max latitude", container = group)
min_lat_select = gedit("4.0", coerce.with = as.numeric, width = 5, container = tmp)
max_lat_select = gedit("15.0", coerce.with = as.numeric, width = 5, container = tmp)

# slider to choose size of locations points
tmp <- gframe("Points / font / arrows size", container = group)
locations_size   <- gspinbutton(from = 0, to = 10, by = 0.5, value = 3, container = tmp)
text_labels_size <- gspinbutton(from = 0, to = 10, by = 0.5, value = 3, container = tmp)
arrow_size       <- gspinbutton(from = 0, to = 10, by = 0.5, value = 1, container = tmp)

# droplist for poly_color ( play with them and choose some nice ones )
tmp <- gframe("Polygons color", container = group)
poly_color <- gdroplist( c("burlywood", "grey20" ), container = tmp, editable = TRUE)

# droplist for boundaries_color ( play with them and choose some nice ones )
tmp <- gframe("Polygon boundaries color", container = group)
boundaries_color <- gdroplist( c("grey60", "grey20" ), container = tmp, editable = T)

# droplist for text_labels_col ( play with them and choose some nice ones )
tmp <- gframe("Text labels color", container = group)
text_labels_col <- gdroplist( c("black", "orange" ), container = tmp, editable = T)

# plot button
tmp <- gframe("BF cutoff / BF test / Plot data", container = group)
specify_bf_cutoff <- gedit("10.0", coerce.with = as.numeric, width = 4, container = tmp) 
add(tmp, gbutton("do BF", handler = RateIndicatorBF) )
add(tmp, gbutton("plot", handler = PlotOnMap) )

# status bar
status_bar <- gstatusbar("Welcome. Expand widget for full screen view.", container = window)


