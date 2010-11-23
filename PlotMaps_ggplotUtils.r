PlotOnMap <- function(h,...) {
 
	#   tryCatch({
		
	min_lon = svalue(MinLon)
	max_lon = svalue(MaxLon)
	min_lat = svalue(MinLat)
	max_lat = svalue(MaxLat)

	if(min_lon<max_lon & min_lat<max_lat) {
		
	poly_color       <- svalue(poly_color)
	boundaries_color <- svalue(boundaries_color)
	text_labels_col  <- svalue(text_labels_col)
	locations_size   <- svalue(locations_size)
	arrow_size       <- svalue(arrow_size)   
	
	world.map <- map_data("world")
	world.map <- world.map[1:5]
	world.map <- subset(world.map, region != "Antarctica")
	world.map <- world.map[-grep("Sea", world.map$region),]
	world.map <- world.map[-grep("Lake", world.map$region),]
	world.map <- world.map[-grep("Island", world.map$region),]
	world.map <- world.map[order(world.map$order), ]
	
	offest <- 25
	
	keepMap <- (world.map$lat >= min_lat - offest) & (world.map$lat <= max_lat + offest) & (world.map$long >= min_lon - offest) & (world.map$long <= max_lon + offest)
	
	mode <- svalue(SelectCoord)
	MapData <- switch(mode,
			"map" = world.map[keepMap,],
			"cartesian" =  world.map[keepMap,],
			"full globe" = world.map
	)	

	svalue(status_bar) <- "Rendering map data..."
	plot(1, col = "white", xlab = "", ylab = "", main = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(min_lon, max_lon), axes = F)
	MAXSTRING <- max(strwidth(locations$location))

	p.map <- ggplot(MapData, aes(long, lat)) 
	p.map <- p.map + geom_polygon(aes(long, lat, group = group ), fill = I(poly_color), size = .2, color = I(boundaries_color)) 
	
	p.map <- switch(mode,
			"map" = p.map + coord_map(projection = "tetra", xlim = c(min_lon, max_lon + MAXSTRING), ylim = c(min_lat, max_lat)),
			"cartesian" =  p.map + coord_cartesian(xlim = c(min_lon, max_lon + MAXSTRING), ylim = c(min_lat, max_lat)),
			# map coordinates cause problem:
			"full globe" = p.map #+ coord_map(projection="mercator")
	)
	
	p.map <- p.map + opts(panel.background = theme_rect(fill = "lightblue", colour = "white")) 
	
	p.map <- p.map + geom_point(data = locations, aes(x = Longitude, y = Latitude), color = I("white"), size = locations_size)	
	# supress arrows at value 0
	if(svalue(arrow_size)!=0) {
		p.map <- p.map + geom_segment(data = out, aes(x = x, y = y, xend = xend, yend = yend, color = BF ), size = I(arrow_size), arrow = arrow(length = unit(0.25,"cm"), ends="last" ) ) 
	}
	p.map <- p.map + geom_text(data = locations, aes(x = Longitude, y = jitter(Latitude, 35), label = location), hjust = -0.1, family = 3, vjust = 0.0, size = svalue(text_labels_size), color = text_labels_col) 

	xgrid <- grid.pretty(c(max_lon, min_lon)) 
	xmaj  <- xgrid[-length(xgrid)]
	ygrid <- grid.pretty(c(max_lat, min_lat)) 
	ymaj  <- ygrid[-length(ygrid)]
	
	p.map <- switch(mode,
			"map" = p.map + scale_y_continuous(breaks = ymaj ) + scale_x_continuous(breaks = xmaj ),
			"cartesian" = p.map + scale_y_continuous(breaks = ymaj ) + scale_x_continuous(breaks = xmaj ),
			"full globe" = p.map + scale_y_continuous(breaks = NA) + scale_x_continuous(breaks = NA)
	)
		
	p.map <- p.map + ylab("") + xlab("")
	print(p.map)
	svalue(status_bar) <- "Done."

	} else {
		svalue(status_bar) <- "Wrong coordinate limits!"
	} #END: if else block
	
	#    }, error = function(e) svalue(status_bar) <- "Could not finish plotting")

} # END:PlotOnMap

