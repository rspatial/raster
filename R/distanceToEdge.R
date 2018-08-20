
.distToEdge <- function(x) {
	xy1 <- xyFromCell(x, 1)
	xy2 <- xyFromCell(x, ncell(x))

	a <- cbind(xFromCol(x, 1), yFromRow(x, 1:nrow(x)))
	b <- cbind(xFromCol(x, 2), yFromRow(x, 1:nrow(x)))
	dX <- pointDistance(a,b,longlat=T)
	m = matrix(1:ncol(x), nrow=nrow(x), ncol=ncol(x), byrow=T)
	m <- m * dX
	
	z <- raster(x)
	z[] <- m
	z2 <- flip(z, 'x')
	z <- min(z, z2)


	dY1 <- pointDistance(xy1, cbind(xy1[1], yFromRow(x, 1:nrow(x))), longlat=T)
	dY2 <- pointDistance(xy2, cbind(xy2[1], yFromRow(x, 1:nrow(x))), longlat=T)
	dY <- pmin(dY1, dY2)

	b <- raster(x)
	b[] <- rep(dY, each=ncol(x))

	d <- min(z,b)
	d
}

