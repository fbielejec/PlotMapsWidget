####################
#---PARSING DATA---#
####################
#################
#---locations---#
#################

GetLocations <- function(filename) {
locations        <- read.table(filename)
names(locations) <- c("location", "Latitude","Longitude")
locations        <- locations[c(1,3,2)]
#without this we have some nasty scope problems
assign("locations", locations, envir = globalenv())
return(locations)
}

############
#---.out---#
############

GetDataset <- function(filename) {
out        <- read.table(filename, skip=3)[c(1,3,5)]
names(out) <- c("BF","from","to")
out$BF     <- as.numeric(do.call("rbind",strsplit(levels(out$BF),"="))[,2])
out$BF     <- factor(round(out$BF,2))
out$x      <- locations$Longitude[match( out$from, locations$location )]
out$y      <- locations$Latitude[match( out$from, locations$location )]
out$xend   <- locations$Longitude[match( out$to, locations$location )]
out$yend   <- locations$Latitude[match( out$to, locations$location )]
#without this we have some nasty scope problems
assign("out", out, envir = globalenv())
return(out)
}

########################
#---PARTIAL MAP DATA---#
########################
PlotOnMap <- function(h,...) {

#   tryCatch({

min_lon = svalue(MinLon)
max_lon = svalue(MaxLon)
min_lat = svalue(MinLat)
max_lat = svalue(MaxLat)

locations = GetLocations( svalue(LocFile) )
out = GetDataset( svalue(OutFile) )

poly_color       <- svalue(poly_color)
boundaries_color <- svalue(boundaries_color)
text_labels_col  <- svalue(text_labels_col)
locations_size   <- svalue(locations_size)
arrow_size       <-svalue(arrow_size)   

  world.map <- map_data("world")
  world.map <- world.map[1:5]
# world.map <- subset(world.map, region != "Antarctica")
  world.map <- world.map[-grep("Sea|Lake", world.map$region),]
  world.map <- world.map[-grep("Island", world.map$region),]
  world.map <- world.map[order(world.map$order), ]

  offest <- 25

keepMap <- (world.map$lat >= min_lat - offest) & (world.map$lat <= max_lat + offest) & (world.map$long >= min_lon - offest) & (world.map$long <= max_lon + offest)
MapData <- world.map[keepMap,]

svalue(status_bar) <- "Rendering map data..."
plot(1, col="white", xlab="", ylab="", main="", xaxt="n", yaxt="n", type="n", xlim=c(min_lon, max_lon), axes = F)
MAXSTRING <- max(strwidth(locations$location))

p.map <- ggplot(MapData, aes(long, lat)) 
p.map <- p.map + geom_polygon(aes(long, lat, group = group ), fill = I(poly_color), size = .2, color = I(boundaries_color)) 

mode <- svalue(SelectCoord)
p.map <- switch(mode,
		"map" = p.map + coord_map(projection = "tetra", xlim = c(min_lon, max_lon + MAXSTRING), ylim = c(min_lat, max_lat)),
		"cartesian" =  p.map + coord_cartesian(xlim = c(min_lon, max_lon + MAXSTRING), ylim = c(min_lat, max_lat))
		)
 
p.map <- p.map + opts(panel.background = theme_rect(fill = "lightblue", colour="white")) 

p.map <- p.map + geom_point(data = locations, aes(x = Longitude, y = Latitude), color = I("white"), size = locations_size)

# I want to supress arrows at value 0
if(svalue(arrow_size)!=0) {
p.map <- p.map + geom_segment(data = out, aes(x = x, y = y, xend = xend, yend = yend, color = BF ), size = I(arrow_size), arrow = arrow(length = unit(0.25,"cm"), ends="last" ) ) 
}

p.map <- p.map + geom_text(data = locations, aes(x = Longitude, y = jitter(Latitude, 35), label = location), hjust = -0.1, family = 3, vjust = 0.0, size = svalue(text_labels_size), color = text_labels_col) 

xgrid <- grid.pretty(c(max_lon, min_lon)) 
xmaj <- xgrid[-length(xgrid)]
ygrid <- grid.pretty(c(max_lat, min_lat)) 
ymaj <- ygrid[-length(ygrid)]
p.map <- p.map + scale_y_continuous(breaks = ymaj ) 
p.map <- p.map + scale_x_continuous(breaks = xmaj )
p.map <- p.map + ylab("")+xlab("") 
svalue(status_bar) <- "Printing to screen..."
print(p.map)
svalue(status_bar) <- "Done!"
 
#    }, error = function(e) svalue(status_bar) <- "Could not finish plotting")
}






