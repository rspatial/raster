# Author: Robert J. Hijmans
# Date :  March 2016
# Version 1.0
# Licence GPL v3

# 'whiches' functions based on code by Data Munger:
# https://stackoverflow.com/questions/36117678/r-raster-how-to-record-ties-using-which-max/36120244#36120244
.whiches <- function(i, fun=min, na.rm=TRUE) {
	w <- getOption('warn')
	on.exit(options('warn'= w))
	options('warn'=-1) 
	m <- which(i == fun(i, na.rm=na.rm))
    sum(m * 10^(rev(seq_along(m)) - 1))
}



setMethod("whiches.min", "RasterStackBrick",  
	function(x) { 

		whichesMin <- function(i) {
			m <- which(i == min(i, na.rm=TRUE))
			sum(m * 10^(rev(seq_along(m)) - 1))
		}
	
		r <- raster(x)
		nl <- nlayers(x)
		if (nl > 9) {
			stop('you can use only use this function for an object with less than 10 layers')
		}
	
		if (canProcessInMemory(x)) {
			x <- values(x)
			d <- dim(x)
			i <- .rowSums(is.na(x), d[1], d[2]) < nl
			y <- rep(NA, nrow(x))
			if (sum(i) > 0) {
				y[i] <- apply(x[i,], 1, whichesMin)
			}	
			return( setValues(r, y) )
		} else {
			tr <- blockSize(x)
			x <- readStart(x)	
			out <- raster(x)
			out <- writeStart(out, '')
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				d <- dim(v)
				j <- .rowSums(is.na(v), d[1], d[2]) < nl
				y <- rep(NA, nrow(v))	
				if (sum(j) > 0) {
					y[j] <- apply(v[j,], 1, whichesMin)
				}	
				out <- writeValues(out, y, tr$row[i])
			}
			out <- writeStop(out)
			x <- readStop(x)
			return(out)
		}
	} 
)
	
	

setMethod("whiches.max", "RasterStackBrick",  
	function(x) { 

		whichesMax <- function(i) {
			m <- which(i == max(i, na.rm=TRUE))
			sum(m * 10^(rev(seq_along(m)) - 1))
		}

		r <- raster(x)
		nl <- nlayers(x)
		if (nl > 9) {
			stop('you can use only use this function for an object with less than 10 layers')
		}
		
		if (canProcessInMemory(x)) {
			x <- values(x)
			d <- dim(x)
			i <- .rowSums(is.na(x), d[1], d[2]) < nl
			y <- rep(NA, nrow(x))	
			if (sum(i) > 0) {
				y[i] <- apply(x[i,], 1, whichesMax)
			}	
			return( setValues(r, y) )
		} else {
			tr <- blockSize(x)
			x <- readStart(x)	
			out <- raster(x)
			out <- writeStart(out, '')
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				d <- dim(v)
				j <- .rowSums(is.na(v), d[1], d[2]) < nl
				y <- rep(NA, nrow(v))	
				if (sum(j) > 0) {
					y[j] <- apply(v[j,], 1, whichesMax)
				}	
				out <- writeValues(out, y, tr$row[i])
			}
			out <- writeStop(out)
			x <- readStop(x)
			return(out)
		}
	} 
)
	
