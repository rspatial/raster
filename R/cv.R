# Author: Robert J. Hijmans 
# Date : October 2008-2011
# Version 1.0
# Licence GPL v3


setGeneric("cv", function(x, ..., aszero=FALSE, na.rm=FALSE)
	standardGeneric("cv"))

	
setMethod('cv', signature(x='ANY'), 
function(x, ..., aszero=FALSE, na.rm=FALSE) {
#  R function to compute the coefficient of variation (expressed as a percentage)
# if there is only a single value, stats::sd = NA. However, one could argue that cv =0. 
# and NA may break the code that receives it.
#The function returns NA if(aszero=FALSE)   else a value of 0 is returned.
	x <- c(x, ...)
	z <- x[!is.na(x)]
	if (length(z) == 0) { 
		return(NA) 
	} else if (na.rm == FALSE & (length(z) < length(x))) { 
		return(NA)	 
	} else if (length(z) == 1 & aszero) { 
		return(0)
	} else {
	# abs to avoid very small (or zero) mean with e.g. -5:5
		x <- mean(abs(z))  
		if (x == 0) {# all values are 0
			return(0)
		} else {
			return(100 * stats::sd(z) / x)
		}
	}	
}
)


setMethod("cv", signature(x='Raster'),
	function(x, ..., aszero=FALSE, na.rm=FALSE){

		dots <- list(...)
		if (length(dots) > 0) {
			x <- stack(.makeRasterList(x, ...))
			add <- .addArgs(...)
		} else {
			add <- NULL
		}
		out <- raster(x)
		
		if (canProcessInMemory(x)) {
			x <- cbind(getValues(x), add)
			x <- setValues(out, apply(x, 1, cv, aszero=aszero, na.rm=na.rm))
			return(x)
		}

		tr <- blockSize(out)
		pb <- pbCreate(tr$n)
		out <- writeStart(out, filename="")
		for (i in 1:tr$n) {
			v <- cbind( getValues( x, row=tr$row[i], nrows=tr$nrows[i] ), add)
			v <- apply(v, 1, cv, aszero=aszero, na.rm=na.rm)
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
		writeStop(out)
	}
)

