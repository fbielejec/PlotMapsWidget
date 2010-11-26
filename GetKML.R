RowAsMatrix <- function(lines, row) {	
	rbind(c(lines[row,]$x, lines[row,]$y), c(lines[row,]$xend, lines[row,]$yend) )
}

GetKML <- function(h,...) {

#	tryCatch({
	
		lines <- out[5:8]
		lines_list <- list()
				
	for(i in 1 : dim(lines)[1] ) {	
		lines_list[[i]] <- Line( RowAsMatrix(lines, i) )
	}

lines <- Lines(lines_list, ID = "BF")
kmlLine(lines, kmlfile = h$file)
svalue(status_bar) <- "Done."	

#     }, error = function(e) svalue(status_bar) <- "Could not save!")

}