###############################
#---HOME BREWED ORGANIC KML---#
###############################
GetKML <- function(h,...) {

	tryCatch({
				
# GOOGLE MAPS      : aa bb gg rr
# rest of the world: aa rr gg bb	
				
				N <- dim(out)[1]
				hsv <- HSV(seq(0, max(as.numeric(levels(out$BF))), length = N + 1), 1, 1)[-1]
				colors <- hex(hsv, gamma = 2.2, fixup = FALSE)			
				google_colors <- rep(NA, N)
	
	for(i in 1 : N) {
		rrggbb = strsplit(colors[i], "")
		aa = "ff"
		rr = paste(rrggbb[[1]][2], rrggbb[[1]][3], sep = "")
		gg = paste(rrggbb[[1]][4], rrggbb[[1]][5], sep = "")
		bb = paste(rrggbb[[1]][6], rrggbb[[1]][7], sep = "")
		google_colors[i] = paste(aa, bb, gg, rr, sep = "")
	}
	
	brew(file = "KmlTemplate.xml", output = h$file )
	svalue(status_bar) <- "Done."	

     }, error = function(e) svalue(status_bar) <- "Could not save!")
}