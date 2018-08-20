# Author: Robert J. Hijmans
# Date : April 2011
# Version 1.0
# Licence GPL v3



.getFilter <- function(w, warn=TRUE) {
	if (!is.matrix(w)) {
		w <- .checkngb(w)
		w <- matrix(1, nrow=w[1], ncol=(w[2]))
		w[ceiling(dim(w)[1]/2), ceiling(dim(w)[2]/2)] <- 0
	} else {
		if (w[ceiling(dim(w)[1]/2), ceiling(dim(w)[2]/2)] != 0) {
			if (warn) {
				warning('central cell of weights matrix (filter) was set to zero')
			}
			w[ceiling(dim(w)[1]/2), ceiling(dim(w)[2]/2)] <- 0
		}		
		stopifnot(all(w >= 0))
	}
	if (min(dim(w) %% 2)==0) {
		stop('dimensions of weights matrix (filter) must be uneven')
	}
	w
}
	
	

Geary <- function(x, w=matrix(c(1,1,1,1,0,1,1,1,1),3,3)) {

	w <- .getFilter(w, warn=FALSE)
	
	i <- trunc(length(w)/2)+1 

	n <- ncell(x) - cellStats(x, 'countNA')
	
	fun <- function(x,...) sum(w*(x-x[i])^2, ...)
	w2 <- w
	w2[] <- 1
	Eij <- cellStats(focal(x, w=w2, fun=fun, na.rm=TRUE, pad=TRUE), sum)	

	if (sum(! unique(w) %in% 0:1) > 0) {
		x <- calc(x, fun=function(x) ifelse(is.na(x), NA ,1))
		W <- focal(x, w=w, na.rm=TRUE, pad=TRUE ) 
	} else {
		w[w==0] <- NA
		W <- focal(x, w=w, fun=function(x, ...){  sum(!is.na(x)) }, pad=TRUE )
	}
	z <- 2 * cellStats(W, sum) * cellStats((x - cellStats(x, mean))^2, sum)
	
	(n-1)*Eij/z
}




GearyLocal <- function(x, w=matrix(c(1,1,1,1,0,1,1,1,1),3,3)) { 
	w <- .getFilter(w)
	i <- trunc(length(w)/2)+1 
	fun <- function(x,...) sum(w*(x-x[i])^2, ...)
	w2 <- w
	w2[] <- 1
	Eij <- focal(x, w=w2, fun=fun, na.rm=TRUE, pad=TRUE)

	s2 <-  cellStats(x, 'sd')^2 
	if (ncell(x) < 1000000) { n <- ncell(x) - cellStats(x, 'countNA' )
	} else { n <- ncell(x) }
	
	s2 <- (s2 * (n-1)) / n 
	Eij / s2
}

