########################
#---PARTIAL MAP DATA---#
########################

# min_lon = -17.0
# max_lon = 7.0
# min_lat = 4.0
# max_lat = 15.5
# 
# offest <- 25
# 
#   world.map <- map_data("world")
#   world.map <- world.map[1:5]
#   world.map <- subset(world.map, region != "Antarctica")
#   world.map <- world.map[-grep("Sea|Lake", world.map$region),]
#   world.map <- world.map[-grep("Island", world.map$region),]
#   world.map <- world.map[order(world.map$order), ]
# 
# keepMap <- (world.map$lat >= min_lat - offest) & (world.map$lat <= max_lat + offest) & (world.map$long >= min_lon - offest) & (world.map$long <= max_lon + offest)
# MapData <- world.map[keepMap,]

#################
#---LIBRARIES---#
#################
library(ggplot2)
library(maps)
library(grid)


####################
#---DEPENDENCIES---#
####################
source("combinatorics.r")

#####################
#---BAYES FACTORS---#
#####################

loc_file = "data/locationCoordinates_H5N1"
log_file = "data/H5N1_HA_discrete_rateMatrix.log"

# hard-coded for now
burn_in    <- 0.1
bf_cutoff  <- 1.5

loc                <- read.table(loc_file, head = FALSE)
indicators         <- read.table(log_file, head = TRUE, sep = "\t")

indicators <- indicators[grep("indicators", names(indicators))]
delete     <- round(dim(indicators)[1] * burn_in)
indicators <- indicators[ - c(1 : delete), ]

K <- dim(loc)[1]
if( ncol(indicators) == K * (K - 1) ) {
  symmetrical = FALSE
} else if (ncol(indicators) == (K * (K - 1)) / 2) {
	symmetrical = TRUE
} else {
	svalue(status_bar) <- "the number of rate indicators does not match the number of locations!"
}

variables    <- as.character(loc[1]$V1)
combinations <- combn(variables, 2)
combinations <- paste(combinations[1, ], combinations[2, ], sep = ":" )

# recognise here, paste accordingly 1 or 2
col_names <- switch(as.character(symmetrical),
		"TRUE"  =  combinations,
		"FALSE" = rep(combinations, 2)
)	
names(indicators) <- col_names

# divide by 1 or 2 accordingly
nr_of_rates_multiplier <- switch(as.character(symmetrical),
		"TRUE"  = 2,
		"FALSE" = 1
)	

qk <- (log(2) + K - 1) / ( (K * (K - 1))/nr_of_rates_multiplier )
pk <- apply(indicators, 2, mean)

out <- (pk/(1 - pk)) / (qk/(1 - qk))

data <- data.frame(
I = pk[which(out > bf_cutoff )],
BF = out[which(out > bf_cutoff )],
from = do.call("rbind", strsplit(names(out[which(out > bf_cutoff )]), ":"))[, 1],
to = do.call("rbind", strsplit(names(out[which(out > bf_cutoff )]), ":"))[, 2]
)
data$x    <- loc$V3[ match(data$from, loc$V1)  ]
data$y    <- loc$V2[ match(data$from, loc$V1)  ]
data$xend <- loc$V3[ match(data$to, loc$V1)  ]
data$yend <- loc$V2[ match(data$to, loc$V1)  ]

data$I    <- round(data$I, 2)
data$BF   <- round(data$BF, 2)
data$I    <- as.factor(data$I)
data$BF   <- as.factor(data$BF)
row.names(data) <- NULL

locations <- data.frame(
location  = with(data, c(as.character(from), as.character(to) ) ),
Longitude = with(data, c(x, xend) ),
Latitude  = with(data, c(y, yend) )
)
locations <- unique(locations)


########################
#---WHOLE WORLD PLOT---#
########################
library(ggplot2)
library(maps)

poly_color       <- "burlywood"
boundaries_color <- "grey20"
arrow_size <- 1.5

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
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank()
)
p.map <- p.map + theme_set(theme_null)
p.map <- p.map + theme(panel.background = element_rect(fill = "lightblue", colour="white")) 
p.map <- p.map + coord_map(xlim=c(-180,180), ylim=c(-60, 80), projection="mercator") 

#TODO: animated line
# library(geosphere)
# inter <- gcIntermediate(c(40, 73), c(52, 20), n=100, addStartEnd=TRUE)  
# inter <- as.data.frame(inter)
p.map + geom_segment(data = data, aes(x = x, y = y, xend = xend, yend = yend, color = BF ), size = I(arrow_size), arrow = arrow(length = unit(0.25,"cm"), ends = "last" ) )
print(p.map)


