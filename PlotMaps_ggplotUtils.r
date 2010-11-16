#############################
#---PARSING DATA FROM KML---#
#############################
getKmlData <- function(filename) {

layer = svalue(KmlLayer)

coords <- getKMLcoordinates(filename, ignoreAltitude=TRUE)

places <- data.frame(do.call("rbind", coords[1:8]))
names(places) <- c("Longitude", "Latitude")

data <- data.frame(do.call("rbind", coords[9:length(coords)]))
rows.to.keep <- as.numeric(row.names(data))%%2
data <- data[rows.to.keep==1,]
names(data) <- c("Longitude", "Latitude")

rates<-readOGR(filename, layer=layer)
rates <- (data.frame(rates))
fun <- function(char) strsplit(char, "\\_")[[1]][1]
rates <- data.frame(apply(rates, 1 ,fun))
names(rates) <- c("rates")

dataset <- cbind(data, rates)
ProperOrder<-reorder(levels(dataset$rates), as.numeric(do.call("rbind", strsplit(levels(dataset$rates), "rate"))[,-1]) )
dataset$rates <- factor(dataset$rates, levels = levels(ProperOrder) )
list(dataset, places)

}

########################
#---PARTIAL MAP DATA---#
########################
mid_range <- function(x) {
median(range(x))
}

PlotOnMap <- function(h,...) {

min_lon = svalue(MinLon)
max_lon = svalue(MaxLon)
min_lat = svalue(MinLat)
max_lat = svalue(MaxLat)

  world.map <- map_data("world")
  world.map <- world.map[1:5]
#  world.map <- subset(world.map, region != "Antarctica")
  world.map <- world.map[-grep("Sea|Lake", world.map$region),]
  world.map <- world.map[-grep("Island", world.map$region),]
  world.map <- world.map[order(world.map$order), ]

offest<-25
keepMap <- (world.map$lat >= min_lat - offest) & (world.map$lat <= max_lat + offest) & (world.map$long >= min_lon - offest) & (world.map$long <= max_lon + offest)
MapData <- world.map[keepMap,]

keepCountries <- (world.map$lat >= min_lat) & (world.map$lat <= max_lat) & (world.map$long >= min_lon) & (world.map$long <= max_lon) 
CountryData <- world.map[keepCountries,]

centres <- ddply(CountryData, c("region"), summarise, lat = mid_range(lat), long = mid_range(long))

svalue(status_bar) <- "Rendering map data..."
plot(1, col="white", xlab="", ylab="", main="", xaxt="n", yaxt="n", type="n", xlim=c(min_lon, max_lon), axes = F)
MAXSTRING <- max(strwidth(centres$region))

p.map <- ggplot(MapData, aes(long, lat)) 
p.map <- p.map + geom_polygon(aes(long, lat, group = group), size = .2, color = "#C4C4C4")


mode <- svalue(SelectCoord)
p.map <- switch(mode,
		"map" = p.map + coord_map(projection = "tetra", xlim=c(min_lon, max_lon + MAXSTRING), ylim=c(min_lat, max_lat)),
		"cartesian" =  p.map + coord_cartesian(xlim=c(min_lon, max_lon + MAXSTRING), ylim=c(min_lat, max_lat))
		)
 
p.map <- p.map + opts(panel.background = theme_rect(fill = "lightblue", colour="white")) 
p.map <- p.map + geom_text(data=centres, aes(long, lat, label = region), hjust=0.0, family=3, vjust=0.0, size=5, color="orange")
p.map <- p.map + geom_path(data=dataset, aes(x=Longitude, y=Latitude, color=rates))
p.map <- p.map + geom_point(data=places, aes(x=Longitude, y=Latitude), color="white")
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
}






