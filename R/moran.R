# Author: Robert J. Hijmans
# Date : April 2011
# Version 1.0
# Licence GPL v3


..moran <- function(x, directions=8) {
	stopifnot(directions %in% c(4,8))
	# not memory safe	
	adj <- adjacent(x, 1:ncell(x), target=1:ncell(x), directions=8, pairs=TRUE)
	z <- x - cellStats(x, mean)
	wZiZj <- stats::na.omit(z[adj[,1]] * z[adj[,2]])
	z2 <- cellStats(z*z, sum)
	NS0 <- (ncell(z)-cellStats(z, 'countNA')) / length(wZiZj)
	mI <- NS0 * sum(wZiZj) / z2
	return(mI)
}



Moran <- function(x, w=matrix(c(1,1,1,1,0,1,1,1,1),3,3) ) {

	z <- x - cellStats(x, mean)
	wZiZj <- focal(z, w=w, fun='sum', na.rm=TRUE, pad=TRUE)
	wZiZj <- overlay(wZiZj, z, fun=function(x,y){ x * y })
	wZiZj <- cellStats(wZiZj, sum)
	z2 <- cellStats(z*z, sum)
	n <- ncell(z) - cellStats(z, 'countNA')
	# weights
	if (sum(! unique(w) %in% 0:1) > 0) {
		zz <- calc(z, fun=function(x) ifelse(is.na(x), NA ,1))
		W <- focal( zz, w=w, fun='sum', na.rm=TRUE, pad=TRUE) 
	} else {
		w2 <- w
		w2[w2==0] <- NA
		W <- focal( z, w=w2, fun=function(x, ...){  as.double(sum(!is.na(x))) }, pad=TRUE)		
	}
	NS0 <- n / cellStats(W, sum)
	mI <- NS0 * wZiZj / z2
	return(mI)
}


MoranLocal <- function(x, w=matrix(c(1,1,1,1,0,1,1,1,1),3,3)) { 
	
	z  <- x - cellStats(x, mean) 
	#weights
	#w <- .getFilter(w)
	if (sum(! unique(w) %in% 0:1) > 0) {
		zz <- calc(z, fun=function(x) ifelse(is.na(x), NA ,1))
		W  <- focal( zz, w=w, na.rm=TRUE, pad=TRUE)		
	} else {
		w2 <- w
		w2[w2==0] <- NA
		W  <- focal( z, w=w2, fun=function(x, ...){ sum(!is.na(x)) }, na.rm=TRUE, pad=TRUE)
	}
	lz <- focal(z, w=w, na.rm=TRUE, pad=TRUE) / W
		
	n <- ncell(x) - cellStats(x, 'countNA')
	s2 <- cellStats(x, 'sd')^2 
	# adjust variance denominator from n-1 to n 
	#s2 <- (s2 * (n-1)) / n 

	(z / s2) * lz
} 


