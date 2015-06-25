############
#---TODO---#
############

#################
#---SCRAPBOOK---#
#################

#################
#---LIBRARIES---#
#################
library(ggplot2)
library(maps)


##############
#---CANCEL---#
##############
library(svSocket)
startSocketServer()

a <- rnorm(10^8)
closeSocketClients(sockets = "all", serverport = 8888)
stopSocketServer(port = 8888)


########################
#---PARTIAL MAP DATA---#
########################

min_lon = -17.0
max_lon = 7.0
min_lat = 4.0
max_lat = 15.5

offest <- 25

  world.map <- map_data("world")
  world.map <- world.map[1:5]
  world.map <- subset(world.map, region != "Antarctica")
  world.map <- world.map[-grep("Sea|Lake", world.map$region),]
  world.map <- world.map[-grep("Island", world.map$region),]
  world.map <- world.map[order(world.map$order), ]

keepMap <- (world.map$lat >= min_lat - offest) & (world.map$lat <= max_lat + offest) & (world.map$long >= min_lon - offest) & (world.map$long <= max_lon + offest)
MapData <- world.map[keepMap,]

plot(1, col="white", xlab="", ylab="", main="", xaxt="n", yaxt="n", type="n", xlim=c(min_lon, max_lon), axes = F)
MAXSTRING <- max(strwidth(locations$location))


poly_color       <- "burlywood"
boundaries_color <- "grey20"
text_labels_col  <- "black"
locations_size   <- 4
text_labels_size <- 4
arrow_size       <- 1

p.map <- ggplot(MapData, aes(long, lat)) 
p.map <- p.map + geom_polygon(aes(long, lat, group = group ), fill = I(poly_color), size = .2, color = I(boundaries_color)) 

p.map <- p.map + coord_map(projection = "tetra", xlim = c(min_lon, max_lon + MAXSTRING), ylim = c(min_lat, max_lat)) 
#p.map <- p.map + coord_cartesian(xlim=c(min_lon, max_lon + MAXSTRING), ylim=c(min_lat, max_lat)) 
p.map <- p.map + opts(panel.background = theme_rect(fill = "lightblue", colour = "white")) 

p.map <- p.map + geom_point(data = locations, aes(x = Longitude, y = Latitude), color = I("white"), size = locations_size)

p.map <- p.map + geom_segment(data = out, aes(x = x, y = y, xend = xend, yend = yend, color = BF ), size = I(arrow_size), 
arrow = arrow(length = unit(0.25, "cm"), ends = "last" ) ) 

p.map <- p.map + geom_text(data = locations, aes(x = Longitude, y = jitter(Latitude, 35), label = location), hjust = -0.1, family = 3, vjust = 0.0, size = text_labels_size, color = text_labels_col) 

xgrid <- grid.pretty(c(max_lon, min_lon)) 
xmaj <- xgrid[-length(xgrid)]
ygrid <- grid.pretty(c(max_lat, min_lat)) 
ymaj <- ygrid[-length(ygrid)]
p.map <- p.map + scale_x_continuous(breaks = xmaj )
p.map <- p.map + scale_y_continuous(breaks = ymaj ) 
p.map <- p.map + ylab("")+xlab("")

print(p.map)

########################
#---WHOLE WORLD PLOT---#
########################
library(ggplot2)
library(maps)


poly_color       <- "burlywood"
boundaries_color <- "grey20"


world.map <- map_data("world")
world.map <- world.map[1:5]
world.map <- world.map[-grep("Antarctica", world.map$region),]
world.map <- world.map[-grep("Greenland", world.map$region),]
world.map <- world.map[-grep("Sea", world.map$region),]
world.map <- world.map[-grep("Lake", world.map$region),]
world.map <- world.map[-grep("Island", world.map$region),]
world.map <- world.map[order(world.map$order), ]

p.map <- ggplot(world.map, aes(long, lat)) 
p.map <- p.map + geom_polygon(aes(long, lat, group = group ), fill = I(poly_color), size = .2, color = I(boundaries_color)) 
p.map <- p.map + ylab("") + xlab("") 
p.map <- p.map + ylim(-55, 85)

theme_null <- theme_update(
		panel.grid.major = theme_blank(),
		panel.grid.minor = theme_blank()
)
p.map <- p.map + theme_set(theme_null)
p.map <- p.map + opts(panel.background = theme_rect(fill = "lightblue", colour="white")) 
p.map <- p.map + coord_map(xlim=c(-180,180), ylim=c(-60, 80), projection="mercator") 

#add line
library(geosphere)
inter <- gcIntermediate(c(40, 73), c(52, 20), n=100, addStartEnd=TRUE)  
inter <- as.data.frame(inter)
p.map <- p.map + geom_line(aes(x = lon, y = lat), data = inter)


print(p.map)


###################
#---BREWING KML---#
###################
library(brew)
brew(file="template.kml", output="output.kml" )

######################
#---MAPPING COLORS---#
######################
N <- dim(out)[1]

hsv <- HSV(seq(0, max(as.numeric(levels(out$BF))), length = N + 1), 1, 1)[-1]
colors <- hex(hsv, gamma = 2.2, fixup = FALSE)	
barplot(rep(1, N), col = colors)


esn <- RGB(
c(238, 134, 230, 1, 46), 
c(114, 186, 2, 166, 30),
c(2, 14, 130, 234, 134),
c("PANTONE 166 U", "PANTONE 368 U", "PANTONE PROCESS MAGENTA U", "PANTONE PROCESS CYAN U", "PANTONE VIOLET U")
)
barplot(rep(1, 5), col = esn)




###########################
#---MERCATOR PROJECTION---#
###########################
library(ggplot2)
library(maps)

world.map <- map_data("world")
world.map <- world.map[1:5]
world.map <- world.map[-grep("Antarctica", world.map$region),]
#world.map <- world.map[-grep("Greenland", world.map$region),]
world.map <- world.map[-grep("Sea", world.map$region),]
world.map <- world.map[-grep("Lake", world.map$region),]
world.map <- world.map[-grep("Island", world.map$region),]
world.map <- world.map[order(world.map$order), ]

MapData <- world.map

poly_color       <- "burlywood"
boundaries_color <- "grey20"
text_labels_col  <- "black"
locations_size   <- 4
text_labels_size <- 4
arrow_size       <- 1


p.map <- ggplot(MapData, aes(long, lat)) 
p.map <- p.map + geom_polygon(aes(long, lat, group = group), fill = I(poly_color), size = .2, color = I(boundaries_color)) 
p.map <- p.map + coord_map(xlim = c( - 180, 180), ylim = c( - 60, 80), projection = "mercator") 

theme_null <- theme_update(
		panel.grid.major = theme_blank(),
		panel.grid.minor = theme_blank()
)
p.map <- p.map + theme_set(theme_null)
p.map <- p.map + opts(panel.background = theme_rect(fill = "lightblue", colour = "white")) 
p.map <- p.map + ylab("") + xlab("")

print(p.map)

