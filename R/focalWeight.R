# Author: Robert J. Hijmans
# Date : June 2013
# Version 1.0
# Licence GPL v3


.circular.weight <- function(rs, d) {
	nx <- 1 + 2 * floor(d/rs[1])
	ny <- 1 + 2 * floor(d/rs[2])
	m <- matrix(ncol=nx, nrow=ny)
	m[ceiling(ny/2), ceiling(nx/2)] <- 1
	if (nx == 1 & ny == 1) {
		return(m)
	} else {
		x <- raster(m, xmn=0, xmx=nx*rs[1], ymn=0, ymx=ny*rs[2], crs="+proj=utm +zone=1 +datum=WGS84")
		d <- as.matrix(distance(x)) <= d
		d / sum(d)
	}
}



.Gauss.weight <- function(rs, sigma) {
	if (length(sigma) == 1) {
		d <- 3 * sigma
	} else {
		d <- sigma[2]
		sigma <- sigma[1]
	}
	nx <- 1 + 2 * floor(d/rs[1])
	ny <- 1 + 2 * floor(d/rs[2])
	m <- matrix(ncol=nx, nrow=ny)
	xr <- (nx * rs[1]) / 2
	yr <- (ny * rs[2]) / 2
	r <- raster(m, xmn=-xr[1], xmx=xr[1], ymn=-yr[1], ymx=yr[1], crs="+proj=utm +zone=1 +datum=WGS84")
	p <- xyFromCell(r, 1:ncell(r))^2
# according to http://en.wikipedia.org/wiki/Gaussian_filter
	m <- 1/(2*pi*sigma^2) * exp(-(p[,1]+p[,2])/(2*sigma^2))
	m <- matrix(m, ncol=nx, nrow=ny, byrow=TRUE)
# sum of weights should add up to 1	
	m / sum(m)
}


.rectangle.weight <- function(rs, d) {
	d <- rep(d, length.out=2)
	nx <- 1 + 2 * floor(d[1]/rs[1])
	ny <- 1 + 2 * floor(d[2]/rs[2])
	m <- matrix(1, ncol=nx, nrow=ny)
	m / sum(m)
}



focalWeight <- function(x, d, type=c('circle', 'Gauss', 'rectangle')) {
	type <- match.arg(type)
	x <- res(x)
	if (type == 'circle') {
		w <- .circular.weight(x, d[1])
		if (fillNA) {
			w[w <= 0] <- NA 
		}
		w	
	} else if (type == 'Gauss') {
		if (!length(d) %in% 1:2) {
			stop("If type=Gauss, d should be a vector of length 1 or 2")
		}
		.Gauss.weight(x, d)
	} else {
		.rectangle.weight(x, d)
	}
}




..simple.circular.weight <- function(radius) {
# based on a function provided by Thomas Cornulier
	x <- -radius:radius
	n <- length(x)
    d <- sqrt(rep(x, n)^2 + rep(x, each=n)^2) <= radius
    matrix(d + 0, n, n) / sum(d)
}

..simple.Gauss.weight <- function(n, sigma) {
# need to adjust for non-square cells to distance.... 
	m <- matrix(ncol=n, nrow=n)
	col <- rep(1:n, n)
	row <- rep(1:n, each=n)
	x <- col - ceiling(n/2)
	y <- row - ceiling(n/2)
# according to http://en.wikipedia.org/wiki/Gaussian_filter
	m[cbind(row, col)] <- 1/(2*pi*sigma^2) * exp(-(x^2+y^2)/(2*sigma^2))
# sum of weights should add up to 1	
	m / sum(m)
}

