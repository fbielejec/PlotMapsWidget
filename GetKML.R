GetKML <- function(h,...) {

	tryCatch({
	
	brew(file = "template.kml", output = h$file )
	svalue(status_bar) <- "Done."	

     }, error = function(e) svalue(status_bar) <- "Could not save!")
}