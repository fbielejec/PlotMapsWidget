#########################################################
#---COMBINATORICS---------------------------------------#
#---functions from combinat package by Scott Chasalow---#
#########################################################
nCm <- function (n, m, tol = 1e-08) 
{
	len <- max(length(n), length(m))
	out <- numeric(len)
	n <- rep(n, length = len)
	m <- rep(m, length = len)
	mint <- (trunc(m) == m)
	out[!mint] <- NA
	out[m == 0] <- 1
	whichm <- (mint & m > 0)
	whichn <- (n < 0)
	which <- (whichm & whichn)
	if (any(which)) {
		nnow <- n[which]
		mnow <- m[which]
		out[which] <- ((-1)^mnow) * Recall(mnow - nnow - 1, mnow)
	}
	whichn <- (n > 0)
	nint <- (trunc(n) == n)
	which <- (whichm & whichn & !nint & n < m)
	if (any(which)) {
		nnow <- n[which]
		mnow <- m[which]
		foo <- function(j, nn, mm) {
			n <- nn[j]
			m <- mm[j]
			iseq <- seq(n - m + 1, n)
			negs <- sum(iseq < 0)
			((-1)^negs) * exp(sum(log(abs(iseq))) - lgamma(m + 
									1))
		}
		out[which] <- unlist(lapply(seq(along = nnow), foo, nn = nnow, 
						mm = mnow))
	}
	which <- (whichm & whichn & n >= m)
	nnow <- n[which]
	mnow <- m[which]
	out[which] <- exp(lgamma(nnow + 1) - lgamma(mnow + 1) - lgamma(nnow - 
							mnow + 1))
	nna <- !is.na(out)
	outnow <- out[nna]
	rout <- round(outnow)
	smalldif <- abs(rout - outnow) < tol
	outnow[smalldif] <- rout[smalldif]
	out[nna] <- outnow
	out
}



combn <- function (x, m, fun = NULL, simplify = TRUE, ...) 
{
	if (length(m) > 1) {
		warning(paste("Argument m has", length(m), "elements: only the first used"))
		m <- m[1]
	}
	if (m < 0) 
		stop("m < 0")
	if (m == 0) 
		return(if (simplify) vector(mode(x), 0) else list())
	if (is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == 
			x) 
		x <- seq(x)
	n <- length(x)
	if (n < m) 
		stop("n < m")
	e <- 0
	h <- m
	a <- 1:m
	nofun <- is.null(fun)
	count <- nCm(n, m, 0.1)
	out <- vector("list", count)
	out[[1]] <- if (nofun) 
				x[a]
			else fun(x[a], ...)
	if (simplify) {
		dim.use <- NULL
		if (nofun) {
			if (count > 1) 
				dim.use <- c(m, count)
		}
		else {
			out1 <- out[[1]]
			d <- dim(out1)
			if (count > 1) {
				if (length(d) > 1) 
					dim.use <- c(d, count)
				else if (length(out1) > 1) 
					dim.use <- c(length(out1), count)
			}
			else if (length(d) > 1) 
				dim.use <- d
		}
	}
	i <- 2
	nmmp1 <- n - m + 1
	mp1 <- m + 1
	while (a[1] != nmmp1) {
		if (e < n - h) {
			h <- 1
			e <- a[m]
			j <- 1
		}
		else {
			h <- h + 1
			e <- a[mp1 - h]
			j <- 1:h
		}
		a[m - h + j] <- e + j
		out[[i]] <- if (nofun) 
					x[a]
				else fun(x[a], ...)
		i <- i + 1
	}
	if (simplify) {
		if (is.null(dim.use)) 
			out <- unlist(out)
		else out <- array(unlist(out), dim.use)
	}
	out
}
