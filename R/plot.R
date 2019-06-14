# Author: Robert J. Hijmans
# Date :  June 2008
# Version 1.0
# Licence GPL v3


if (!isGeneric("plot")) {
	setGeneric("plot", function(x,y,...)
		standardGeneric("plot"))
}	


setMethod("plot", signature(x='Raster', y='ANY'), 
	function(x, y, maxpixels=500000, col, alpha=NULL, colNA=NA, add=FALSE, ext=NULL, useRaster=TRUE, interpolate=FALSE, addfun=NULL, nc, nr, maxnl=16, main, npretty=0, ...)  {

		hasNoCol <- missing(col)
		if (hasNoCol) {
			col <- rev(terrain.colors(255))
		}
			
		if (!is.null(alpha)) {	
			if (inherits(alpha, 'RasterLayer')) {
				if (!compareRaster(x, alpha)) {
					alpha <- NULL
				}
			} else {
				alpha <- pmax(pmin(alpha, 1), 0)
				if (length(alpha) == 1) {
					alpha <- alpha * 255 + 1
					a <- c(0:9, LETTERS[1:6])
					alpha <- paste(rep(a, each=16), rep(a, times=16), sep='')[alpha]
					col <- paste(substr(col, 1, 7), alpha, sep="")
					alpha <- NULL
				}
			}
		}
		
		nl <- nlayers(x)
		if (nl == 0) {
			stop('Raster object has no cell values')
		}

		if (nl == 1) {
			if (inherits(x, 'RasterStackBrick')) {
				x <- raster(x, 1)
			}
			facvar <- 0
			if (!missing(y)) {
				if (is.factor(x)) {
					facvar <- max(y, 0)
				} 
			}
	
			
			if ( (length(x@legend@colortable) > 0) & hasNoCol) {
				.plotCT(x, maxpixels=maxpixels, ext=ext, interpolate=interpolate, main=main, add=add, addfun=addfun, ...)
			} else if (! useRaster) {
				.plotraster(x, col=col, maxpixels=maxpixels, add=add, ext=ext, main=main, addfun=addfun, ...) 
			} else {
				.plotraster2(x, col=col, maxpixels=maxpixels, add=add, ext=ext, interpolate=interpolate, colNA=colNA, main=main, addfun=addfun, facvar=facvar, alpha=alpha, npretty=npretty, ...) 
				#.plot2(x, col=col, maxpixels=maxpixels, ...)
			}
			return(invisible(NULL))
		}
	
		if (missing(main)) {
			main <- names(x)
		}
		if (missing(y)) {
			y <- 1:nl
			if (length(y) > maxnl) {
				y <- 1:maxnl
			}
		} else {
			if (is.character(y)) {
				y <- match(y, names(x))
			}
			y <- unique(as.integer(round(y)))
			y <- stats::na.omit(y)
		}
		
		
		if (length(y) == 1) {
			x <- raster(x, y)
			if ( (length(x@legend@colortable) > 0) & hasNoCol) {
				.plotCT(x, maxpixels=maxpixels, ext=ext, interpolate=interpolate, main=main[y], addfun=addfun, ...)
			} else if (useRaster) {
				.plotraster2(x, col=col, colNA=colNA, maxpixels=maxpixels, main=main[y], ext=ext, interpolate=interpolate, addfun=addfun, , alpha=alpha, ...) 
			} else {
				.plotraster(x, col=col, maxpixels=maxpixels, main=main[y], ext=ext, addfun=addfun, ...) 
			}
			
		} else {

			nl <- length(y)
			if (missing(nc)) {
				nc <- ceiling(sqrt(nl))
			} else {
				nc <- max(1, min(nl, round(nc)))
			}
			if (missing(nr)) {
				nr <- ceiling(nl / nc)
			} else {
				nr <- max(1, min(nl, round(nr)))
				nc <- ceiling(nl / nr)
			}
		
			old.par <- graphics::par(no.readonly = TRUE) 
			on.exit(graphics::par(old.par))
			graphics::par(mfrow=c(nr, nc), mar=c(2, 2, 2, 4))
			xa='n'
			rown=1
			coln=0
			maxpixels=maxpixels/nl
			
			if (missing(main)) {
				main <- names(x)
			}			
			
			for (i in 1:nl) {
				coln = coln + 1
				if (coln > nc) {
					coln <- 1
					rown = rown + 1
				}
				if (rown==nr) xa='s'
				if (coln==1) ya='s' else ya='n'
				
				obj <- raster(x, y[i])
				if ((length(obj@legend@colortable) > 0) & hasNoCol) {
					.plotCT(obj, maxpixels=maxpixels, ext=ext, interpolate=interpolate, main=main, addfun=addfun, ...)
				} else if (useRaster) {
					.plotraster2(obj, col=col, maxpixels=maxpixels, xaxt=xa, yaxt=ya, main=main[y[i]], ext=ext, interpolate=interpolate, colNA=colNA, addfun=addfun, alpha=alpha, ...) 
				} else {
					.plotraster(obj, col=col, maxpixels=maxpixels, xaxt=xa, yaxt=ya, main=main[y[i]], ext=ext, interpolate=interpolate, addfun=addfun, ...) 
				}
			}		
		}
		return(invisible(NULL))
	}
)	


setMethod("lines", signature(x='RasterLayer'),
function(x, ...) {
	if(prod(dim(x)) < 50000) {
		stop('too many lines')
	}
	x <- as(x, 'SpatialPolygons')
	lines(x, ...)
}
)
