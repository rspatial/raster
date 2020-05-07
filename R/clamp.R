# Author: Robert J. Hijmans
# Date : July 2013
# Version 1.0
# Licence GPL v3

setMethod("clamp", signature(x="Raster"), 
function(x, lower=-Inf, upper=Inf, useValues=TRUE, filename="", ...) {

	if (!hasValues(x)) return(x)
	useValues <- as.integer(useValues)
	
	byCol = FALSE
	nl <- nlayers(x)
	if (nl == 1) {
		if ((length(lower) > 1) | (length(upper) > 1)) {
			warning("only the first element of lower/upper is used")
			lower <- lower[1]
			upper <- upper[1]
		} 
		stopifnot(lower <= upper)
		out <- raster(x)
		crange <- c(lower, upper)
	} else {
		if ((length(lower) > 1) | (length(upper) > 1)) {
			lower = rep_len(lower, nl)
			upper = rep_len(upper, nl)
			stopifnot(all (lower <= upper) )
			byCol = TRUE
			crange <- cbind(lower, upper)
		} else {
			stopifnot(lower <= upper)
			crange <- c(lower, upper)
		}
		out <- brick(x, values=FALSE)
	}
	names(out) <- names(x)

	if (byCol) {
		if (canProcessInMemory(out)) {
			v <- values(x)
			for (i in 1:ncol(v)) {
				v[,i] <- .clamp(v[,i], crange[i,], useValues)
			}
			out <- setValues(out, v) 
			if (filename != "") {
				writeRaster(out, filename, ...)
			}
		} else {
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label="clamp", ...)
			out <- writeStart(out, filename=filename, ...)
			
			for (i in 1:tr$n) {
				vals <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				for (j in 1:ncol(vals)) {
					vals[,j] <- .clamp(vals[,j], crange[j,], useValues)
				}
				out <- writeValues(out, vals, tr$row[i])
				pbStep(pb, i)
			}
			out <- writeStop(out)
			pbClose(pb)
		}
	} else {
		if (canProcessInMemory(out)) {
			out <- setValues(out, .clamp(values(x), crange, useValues)) 
			if (filename != "") {
				writeRaster(out, filename, ...)
			}
		} else {
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label="clamp", ...)
			out <- writeStart(out, filename=filename, ...)
			
			for (i in 1:tr$n) {
				vals <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				vals <- .clamp(vals, crange, useValues)
				if (nl > 1) {
					vals <- matrix(vals, ncol=nl)
				}
				out <- writeValues(out, vals, tr$row[i])
				pbStep(pb, i)
			}
			out <- writeStop(out)
			pbClose(pb)
		}
	}
	return(out)
}
)


setMethod("clamp", signature(x="numeric"), 
function(x, lower=-Inf, upper=Inf, ...) {
	stopifnot(lower <= upper)
	x[x < lower] <- lower
	x[x > upper] <- upper
	return(x)
}
)


