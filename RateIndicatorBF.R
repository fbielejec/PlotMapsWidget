####################
#---PROGRAM FLOW---#
####################
RateIndicatorBF <- function(h,...){

	   tryCatch({
# maybe move reading data to different handler				   
svalue(status_bar) <- "Computing..."
loc                <- read.table(svalue(loc_file), head = FALSE)
indicators         <- read.table(svalue(log_file), head = TRUE, sep = "\t")

# hard-coded for now
burn_in    <- 0.1
bf_cutoff  <- svalue(specify_bf_cutoff)

indicators    <- indicators[grep("indicators", names(indicators))]
delete <- round(dim(indicators)[1] * burn_in)
indicators    <- indicators[-c(1:delete), ]


K <- dim(loc)[1]
if( ncol(indicators) == K*(K - 1) ) {
	symmetrical = FALSE
} else if (ncol(indicators) == (K*(K - 1)) / 2) {
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

assign("out", data, envir = globalenv() )
assign("locations", locations, envir = globalenv() )

svalue(status_bar) <- "Done."

    }, error = function(e) svalue(status_bar) <- "Could not compute!")

} # END: RateIndicatorBF
