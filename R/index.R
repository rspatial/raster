# Author: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("[", c("Extent", "numeric", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	x <- as.vector(x)
	x[i]
})

setMethod("[", c("Extent", "missing", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	as.vector(x)
})

setMethod("[", c("Raster", "Spatial", "missing"),
function(x, i, j, ... ,drop=TRUE) {

	if (inherits(i, 'SpatialPoints')) {
		i <- coordinates(i)
		i <- cellFromXY(x, i)
		.doExtract(x, i, ..., drop=drop)
	
	} else {
		if (drop) {
			extract(x, i, ...)
		} else {
			x <- crop(x, i, ...)
			rasterize(i, x, mask=TRUE, ...)
		}
	}
})



setMethod("[", c("Raster", "RasterLayer", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	
	if (! hasValues(i) ) {
		i <- extent(i)
		methods::callNextMethod(x, i=i, ..., drop=drop)
	
	} else if (compareRaster(x, i, stopiffalse=FALSE, showwarning=FALSE)) {
		i <- which( as.logical( getValues(i) ) )
		.doExtract(x, i, drop=drop)

	} else {

		i <- intersect(extent(x), extent(i))
		methods::callNextMethod(x, i=i, ..., drop=drop)
	}
})


setMethod("[", c("Raster", "Extent", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	if (drop) {
		return( extract(x, i) )
	} else {
		return( crop(x, i) )
	}
} )	
	
setMethod("[", c("Raster", "missing", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	if (drop) {
		return(getValues(x))
	} else {
		return(x)
	}
})

setMethod("[", c("Raster", "numeric", "numeric"),
function(x, i, j, ... ,drop=TRUE) {
		i <- cellFromRowColCombine(x, i, j)
		.doExtract(x, i, drop=drop)
	}
)

setMethod("[", c("Raster", "missing", "numeric"),
function(x, i, j, ... ,drop=TRUE) {
	j <- cellFromCol(x, j)
	.doExtract(x, j, drop=drop)
})



setMethod("[", c("Raster", "numeric", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	theCall <- sys.call(-1)
	narg <- length(theCall) - length(match.call(call=sys.call(-1)))
	if (narg > 0) {
		i <- cellFromRow(x, i)
	} 
	.doExtract(x, i, drop=drop)
})



setMethod("[", c("Raster", "matrix", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	if (ncol(i) == 2) {
		i <- cellFromRowCol(x, i[,1], i[,2])
	} else {
		i <- as.vector(i)
	}
	.doExtract(x, i, drop=drop)
})



setMethod("[", c("Raster", "logical", "missing"),
function(x, i, j, ... , drop=TRUE) {
	theCall <- sys.call(-1)
	narg <- length(theCall) - length(match.call(call=sys.call(-1)))
	if (narg > 0) {
		stop('logical indices are only accepted if only the first index is used')
	}
	i <- which(i)
	.doExtract(x, i, drop=drop)
})


.doExtract <- function(x, i, drop) {	
	if (length(i) < 1) return(NULL) 
	nacount <- sum(is.na(i))
	if (nacount > 0) {
		warning('some indices are invalid (NA returned)')
	}	
	if (!drop) {
		i <- stats::na.omit(i)
		r <- rasterFromCells(x, i, values=FALSE)
		if (nlayers(x) > 1) {
			r <- brick(r)
			if (hasValues(r)) {
				newi <- cellFromXY(r, xyFromCell(x, i))
				v <- matrix(NA, nrow=ncell(r), ncol=nlayers(x))			
				v[newi,] <- .cellValues(x, i)
				r <- setValues(r, v)
			} 
			return(r)
		} else {
			if (hasValues(r)) {
				newi <- cellFromXY(r, xyFromCell(x, i))
				r[newi] <- .cellValues(x, i)
			}
			return(r)
		}
	
	} else {
		if (! hasValues(x) ) {	
			stop('no data associated with this Raster object')
		}
		return( .cellValues(x, i) )
		
	} 
}

