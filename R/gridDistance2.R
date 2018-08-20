# Author: Robert J. Hijmans
# Date :  December 2011
# Version 1.0
# Licence GPL v3


.gridDistance2 <- function(x, filename='', ...) {

# currently only works for planar data! 

	rs <- res(x)
	xdist <- rs[1]
	ydist <- rs[2]
	xydist <- sqrt(xdist^2 + ydist^2)
	z1 <- z2 <- raster(x)
	nc <- ncol(z1)
	filename <- trim(filename)
	
	if (canProcessInMemory(z1)) {
		f <- rep(Inf, nc)
		z1a <- z2a <- raster(x)
		x <- getValues(x)
		a <- as.integer(dim(z1))
		b <- c(xdist, ydist, xydist)
		z1a[] <- .Call('_broom', x, f, a , b, as.integer(1), NAOK=TRUE, PACKAGE='raster')
		z2a[] <- .Call('_broom', x, f, a , b, as.integer(0), NAOK=TRUE, PACKAGE='raster')
		x <- min(z1a, z2a)
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
	} else {
		tr <- blockSize(z1)
		pb <- pbCreate(tr$n*2, ...)
		z1 <- writeStart(z1, rasterTmpFile())
		i <- 1
		v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
		f <- rep(Inf, nc)
		z <- .Call('_broom', v, 	f, as.integer(c(tr$nrows[i], nc)), c(xdist, ydist, xydist), as.integer(1), NAOK=TRUE, PACKAGE='raster')
		z1 <- writeValues(z1, z, tr$row[i])
		f <- z[(length(z)-nc+1):length(z)]
		for (i in 2:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			z <- .Call('_broom', v, f, as.integer(c(tr$nrows[i], nc)), c(xdist, ydist, xydist), as.integer(1), NAOK=TRUE, PACKAGE='raster')
			z1 <- writeValues(z1, z, tr$row[i])
			f <- z[(length(z)-nc+1):length(z)]
			pbStep(pb, i)
		}
		z1 <- writeStop(z1)
		
		z2 <- writeStart(z2, rasterTmpFile())
		i <- tr$n
		v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
		f <- rep(Inf, nc)
		z <- .Call('_broom', v, 	f, as.integer(c(tr$nrows[i], nc)), c(xdist, ydist, xydist), as.integer(0), NAOK=TRUE, PACKAGE='raster')
		z2 <- writeValues(z2, z, tr$row[i])
		f <- z[1:nc]
		for (i in (tr$n-1):1) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			z <- .Call('_broom', v, f, as.integer(c(tr$nrows[i], nc)), c(xdist, ydist, xydist), as.integer(0), NAOK=TRUE, PACKAGE='raster')
			z2 <- writeValues(z2, z, tr$row[i])
			f <- z[1:nc]
			pbStep(pb, i)
		}
		z2 <- writeStop(z2)
		x <- calc(stack(z1, z2), fun=min, filename=filename)
		file.remove(filename(z1))
		file.remove(filename(z2))
	}
	return(x)
}

