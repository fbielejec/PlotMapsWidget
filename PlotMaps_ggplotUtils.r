PlotOnMap <- function(h,...) {
 
	#   tryCatch({
		
	MinLon <- svalue(min_lon)
	MaxLon <- svalue(max_lon)
	MinLat <- svalue(min_lat)
	MaxLat <- svalue(max_lat)

	if(MinLon < MaxLon & MinLat < MaxLat) {
		
	poly_color       <- svalue(poly_color)
	boundaries_color <- svalue(boundaries_color)
	text_labels_col  <- svalue(text_labels_col)
	locations_size   <- svalue(locations_size)
	arrow_size       <- svalue(arrow_size)   
	
	world.map <- map_data("world")
	world.map <- world.map[1:5]
	world.map <- subset(world.map, region != "Antarctica")
	world.map <- world.map[-grep("Sea", world.map$region), ]
	world.map <- world.map[-grep("Lake", world.map$region), ]
	world.map <- world.map[-grep("Island", world.map$region), ]
	world.map <- world.map[order(world.map$order), ]
	
	offest <- 25
	
	keepMap <- (world.map$lat >= MinLat - offest) & (world.map$lat <= MaxLat + offest) & 
			   (world.map$long >= MinLon - offest) & (world.map$long <= MaxLon + offest)
	
	coord_mode <- svalue(select_coord)
	MapData <- switch(coord_mode,
			"map" = world.map[keepMap, ],
			"cartesian" =  world.map[keepMap, ],
			"full globe" = world.map
	)	

	svalue(status_bar) <- "Rendering map data..."
	plot(1, col = "white", xlab = "", ylab = "", main = "", xaxt = "n", yaxt = "n", type = "n", 
			xlim = c(MinLon, MaxLon), axes = F)
	MAXSTRING <- max(strwidth(locations$location))

	p.map <- ggplot(MapData, aes(long, lat)) 
	p.map <- p.map + geom_polygon(aes(long, lat, group = group ), fill = I(poly_color), size = .2, color = I(boundaries_color)) 
	
	p.map <- switch(coord_mode,
			"map" = p.map + coord_map(projection = "tetra", xlim = c(MinLon, MaxLon + MAXSTRING), ylim = c(MinLat, MaxLat)),
			"cartesian" =  p.map + coord_cartesian(xlim = c(MinLon, MaxLon + MAXSTRING), ylim = c(MinLat, MaxLat)),
			# map coordinates cause problem:
			"full globe" = p.map #+ coord_map(projection="mercator")
	)
	

	
	p.map <- p.map + geom_point(data = locations, aes(x = Longitude, y = Latitude), color = I("white"), size = locations_size)	
	
	# supress arrows at value 0
	if(svalue(arrow_size) != 0) {		
		arrow_mode <- svalue(select_arrow_tips)
		p.map <- switch(arrow_mode, 
				"last"  = p.map + geom_segment(data = out, aes(x = x, y = y, xend = xend, yend = yend, color = BF ), size = I(arrow_size), arrow = arrow(length = unit(0.25,"cm"), ends="last" ) ), 
				"first" = p.map + geom_segment(data = out, aes(x = x, y = y, xend = xend, yend = yend, color = BF ), size = I(arrow_size), arrow = arrow(length = unit(0.25,"cm"), ends="first" ) ),
				"both"  = p.map + geom_segment(data = out, aes(x = x, y = y, xend = xend, yend = yend, color = BF ), size = I(arrow_size), arrow = arrow(length = unit(0.25,"cm"), ends="both" ) ), 
				"none"  = p.map + geom_segment(data = out, aes(x = x, y = y, xend = xend, yend = yend, color = BF ), size = I(arrow_size) )
		)
	}
	p.map <- p.map + geom_text(data = locations, aes(x = Longitude, y = jitter(Latitude, 35), label = location), hjust = -0.1, family = 3, vjust = 0.0, size = svalue(text_labels_size), color = text_labels_col) 

	
	if(coord_mode %in% c("map", "cartesian") ) {	
		xgrid <- grid.pretty(c(MaxLon, MinLon)) 
		xmaj  <- xgrid[-length(xgrid)]
		ygrid <- grid.pretty(c(MaxLat, MinLat)) 
		ymaj  <- ygrid[-length(ygrid)]
}


	if(coord_mode == "full globe") {	
		theme_null <- theme_update(
			panel.grid.major = theme_blank(),
			panel.grid.minor = theme_blank()
	)
}


	p.map <- switch(coord_mode,
			"map" = p.map + scale_y_continuous(breaks = ymaj ) + scale_x_continuous(breaks = xmaj ),
			"cartesian" = p.map + scale_y_continuous(breaks = ymaj ) + scale_x_continuous(breaks = xmaj ),
			"full globe" = p.map + theme_set(theme_null)
	)
	
	p.map <- p.map + opts(panel.background = theme_rect(fill = "lightblue", colour = "white")) 	
	p.map <- p.map + ylab("") + xlab("")
	
	print(p.map)
	
	svalue(status_bar) <- "Done."

	} else {
		svalue(status_bar) <- "Wrong coordinate limits!"
	} #END: if else block
	
	#    }, error = function(e) svalue(status_bar) <- "Could not finish plotting")

} # END:PlotOnMap

