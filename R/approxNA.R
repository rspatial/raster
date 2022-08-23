# Author: Robert J. Hijmans
# Date : February 2012
# Version 1.0
# Licence GPL v3


setMethod('approxNA', signature(x='RasterStackBrick'), 
function(x, filename="", method="linear", yleft, yright, rule=1, f=0, ties=mean, z=NULL, NArule=1, ...) { 

	filename <- trim(filename)
	out <- brick(x, values=FALSE)
	nl <- nlayers(out)
	if (nl < 2) {
		warning('cannot interpolate with a single layer')
		return(x)
	}
	
	if (is.null(z)) {
		xout <- getZ(x)
		if (is.null(xout)) {
			xout <- 1:nl
		} else if (length(xout)!= nl) {
			stop('length of values returned by getZ(x) does not match the number of layers of x')
		}
	} else {
		if (length(z)!= nl) {
			stop('length of z does not match the number of layers of x')
		}
		xout <- z		
	}
	
	ifelse((missing(yleft) & missing(yright)), ylr <- 0L, ifelse(missing(yleft), ylr <- 1L, ifelse(missing(yright), ylr <- 2L, ylr <- 3L)))
	
	if (canProcessInMemory(x)) {
		x <- getValues(x)
		s <- rowSums(is.na(x))
		if (isTRUE(NArule==1)) {
			j <- s == (nl-1) # one non-NA only
			if (length(j) > 0 ) {
				x[j, ] <- apply(x[j, ,drop=FALSE], 1, max, na.rm=TRUE)
			}
		}
		i <- s < (nl-1) # at least two
		if (length(i) > 0 ) {
			if (ylr==0) {
				x[i,] <- t(apply(x[i,,drop=FALSE], 1, function(x) stats::approx(x=xout, y=x, xout=xout, method=method, rule=rule, f=f, ties=ties)$y ))
			} else if (ylr==1) {
				x[i,] <- t(apply(x[i,,drop=FALSE], 1, function(x) stats::approx(x=xout, y=x, xout=xout, method=method, yright=yright, rule=rule, f=f, ties=ties)$y ))			
			} else if (ylr==2) {
				x[i,] <- t(apply(x[i,,drop=FALSE], 1, function(x) stats::approx(x=xout, y=x, xout=xout, method=method, yleft=yleft, rule=rule, f=f, ties=ties)$y ))						
			} else {
				x[i,] <- t(apply(x[i,,drop=FALSE], 1, function(x) stats::approx(x=xout, y=x, xout=xout, method=method, yright=yright, yleft=yleft, rule=rule, f=f, ties=ties)$y ))
			}
		} else {
			warning('no NA values to approximate')
		}
		x <- setValues(out, x)
		if (filename != '') {
			x <- writeRaster(x, filename=filename, ...)
		}
		return(x)
	} 
	
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, label='approxNA', ...)
	out <- writeStart(out, filename=filename, ...)
    nc <- ncol(out)
	for (j in 1:tr$n) {
		v <- getValues(x, row=tr$row[j], nrows=tr$nrows[j])
		s <- .rowSums(is.na(v), nrow(v), nl)

		if (isTRUE(NArule==1)) {
			k <- s == (nl-1) # one non-NA only
			if (length(k) > 0 ) {
				v[k, ] <- apply(v[k,,drop=FALSE ], 1, max, na.rm=TRUE)
			}
		}
		i <- (s < nl-1) # need at least two
		if (length(i) > 0 ) {
			if (ylr==0) {
				v[i,] <- t( apply(v[i,,drop=FALSE], 1, function(x) stats::approx(x=xout, y=x, xout=xout, method=method, rule=rule, f=f, ties=ties)$y ) )
			} else if (ylr==1) {
				v[i,] <- t( apply(v[i,,drop=FALSE], 1, function(x) stats::approx(x=xout, y=x, xout=xout, method=method, yright=yright, rule=rule, f=f, ties=ties)$y ) )
			} else if (ylr==2) {
				v[i,] <- t( apply(v[i,,drop=FALSE], 1, function(x) stats::approx(x=xout, y=x, xout=xout, method=method, yleft=yleft, rule=rule, f=f, ties=ties)$y ) )
			} else {
				v[i,] <- t( apply(v[i,,drop=FALSE], 1, function(x) stats::approx(x=xout, y=x, xout=xout, method=method, yright=yright, yleft=yleft, rule=rule, f=f, ties=ties)$y ) )
			}
		}
		out <- writeValues(out, v, start=tr$row[j])
		pbStep(pb)
	}
	
	pbClose(pb)
	out <- writeStop(out)
	return(out)
}
)

