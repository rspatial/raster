# Author: Robert J. Hijmans
# Date :  October 2008
# Version 0.9
# Licence GPL v3



setMethod('origin', signature(x='BasicRaster'), 
function(x, ...) {
	e <- x@extent
	r <- res(x)
	x <- e@xmin - r[1]*(round(e@xmin / r[1]))
	y <- e@ymax - r[2]*(round(e@ymax / r[2]))
	
	if (isTRUE(all.equal((r[1] + x), abs(x)))) {
		x <- abs(x)
	}
	if (isTRUE(all.equal((r[2] + y), abs(y)))) {
		y <- abs(y)
	}
	return(c(x, y))
}
)


if (!isGeneric("origin<-")) {
	setGeneric("origin<-", function(x, value)
		standardGeneric("origin<-"))
}

setMethod("origin<-", signature('BasicRaster'), 
	function(x, value) {
		value <- rep(value, length.out=2)
		dif <- value - origin(x)
		res <- res(x)
		dif[1] <- dif[1] %% res[1]
		dif[2] <- dif[2] %% res[2]
		for (i in 1:2) {
			if (dif[i] < 0) {
				if ((dif[i] + res[i]) < abs(dif[i])) {
					dif[i] <- dif[i] + res[i]
				}
			} else {
				if (abs(dif[i] - res[i]) < dif[i]) {
					dif[i] <- dif[i] - res[i]
				}
			}
		}
		e <- extent(x)
		e@xmin <- e@xmin + dif[1]
		e@xmax <- e@xmax + dif[1]		
		e@ymin <- e@ymin + dif[2]
		e@ymax <- e@ymax + dif[2]		
		x@extent <- e
		return(x)
	}
)

