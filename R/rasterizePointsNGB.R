


.p2r <- function(p, r=1, x, field, fun, ...) {
	points <- .pointsToMatrix(p)
	field <- .getPutVals(p, field, nrow(points), mask=FALSE)
	x <- raster(x)
	bf <- .xyvBuf(x, points, r, fun=NULL, na.rm=TRUE, cellnumbers=TRUE, small=TRUE, onlycells=TRUE)
	bf <- do.call(rbind, bf)
	bf <- bf[order(bf[,2]), ]
	field <- data.frame(field, value=1:NROW(field))
	bf <- merge(bf, field, by='value')
	cellvs <- tapply(bf$field, bf[, 'cell', drop=F], fun)
	cellvs <- cbind(as.numeric(names(cellvs)), do.call(rbind, cellvs))
	if (ncol(cellvs) > 2) {
		x <- brick(x, nl=ncol(cellvs)-1)
	}
	x[cellvs[,1]] <- cellvs[,-1]
	x
}


