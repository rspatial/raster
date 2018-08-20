# author Josh Gray
# http://spatiallyexplicit.wordpress.com/2011/06/07/crop-circles/
# minor modifications by Robert Hijmans
# Note: these functions only work (correctly) for single layer objects 

.linStretchVec <- function (x) {
    v <- stats::quantile(x, c(0.02, 0.98), na.rm = TRUE)
    temp <- (255 * (x - v[1]))/(v[2] - v[1])
    temp[temp < 0] <- 0
    temp[temp > 255] <- 255
    return(temp)
}


.linStretch <- function (x) {
    v <- stats::quantile(x, c(0.02, 0.98), na.rm = TRUE)
    temp <- calc(x, fun = function(x) (255 * (x - v[1]))/(v[2] - v[1]))
    temp[temp < 0] <- 0
    temp[temp > 255] <- 255
    return(temp)
}

# Histogram equalization stretch
.eqStretch <- function(x){
	ecdfun <- stats::ecdf(getValues(x))
	return( calc(x, fun=function(x) ecdfun(x)*255) )
}

.eqStretchVec <- function(x){
	ecdfun <- stats::ecdf(x)
	ecdfun(x)*255
}


# This one works for multilayer objects
# Linear stretch between min and max values
stretch <- function(x, minv=0, maxv=255, minq=0, maxq=1, filename='', ...) {
	minq <- max(0,minq)
	maxq <- min(1,maxq)
	stopifnot(minq < maxq)
	maxv <- maxv[1]
	minv <- minv[1]
	stopifnot(maxv > minv)
	nl <- nlayers(x)
	q <- quantile(x, c(minq, maxq), na.rm=TRUE)
	
	if (nl == 1) {	
		out <- raster(x)
		if (canProcessInMemory(out)) {
			x <- getValues(x)
			x <- (maxv*(x-q[1]))/(q[2]-q[1])
			x[x < minv] <- minv
			x[x > maxv] <- maxv
			out <- setValues(out, x)
		} else {
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='stretch', ...)		
			out <- writeStart(out, filename, ...)
			mult <- maxv / (q[2]-q[1])
			for (i in 1:tr$n) {
				v <- getValues(x, tr$row[i], tr$nrows[i])
				v <- mult*(v-q[1])
				v[v < 0] <- 0
				v[v > maxv] <- maxv
				out <- writeValues(out, v, tr$row[i])
			}
			out <- writeStop(out)
		}
	} else {
		out <- brick(x, values=FALSE)
		mxrnge <- maxv/(q[,2]-q[,1])
		fun <- function(x) mxrnge*(x-q[,1])
		if (canProcessInMemory(out)) {
			x <- getValues(x)
			x <- t(apply(x, 1, fun))
			x[x < 0] <- 0
			x[x > maxv] <- maxv
			out <- setValues(out, x)
		} else {
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='stretch', ...)		
			out <- writeStart(out, filename, ...)
			for (i in 1:tr$n) {
				v <- getValues(x, tr$row[i], tr$nrows[i])
				v <- t(apply(v, 1, fun))
				v[v < 0] <- 0
				v[v > maxv] <- maxv
				out <- writeValues(out, v, tr$row[1])
			}
			out <- writeStop(out)
		}
	}
	return(out)
}

