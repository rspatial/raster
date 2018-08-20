# Author: Robert J. Hijmans
# Date :  April 2009
# Version 0.9
# Licence GPL v3


setMethod("image", signature(x='RasterLayer'), 
	function(x, maxpixels=500000, useRaster=TRUE, ...)  {
#		coltab <- x@legend@colortable
#		if (is.null(coltab) | length(coltab) == 0 | is.null(list(...)$col)) {
#			colortab <- FALSE		
#		}
#		if (missing(main)) {	main <- names(x) 	}

		x <- sampleRegular(x, maxpixels, asRaster=TRUE, useGDAL=TRUE)
		y <- yFromRow(x, nrow(x):1)
# drop=F fix by Daniel Schlaepfer for single row image
		value <- t(as.matrix(x)[nrow(x):1, ,drop=FALSE])
		x <- xFromCol(x,1:ncol(x))
#		if (colortab) {
#			image(x=x, y=y, z=value, col=coltab[value], useRaster=useRaster, ...)
#		} else {

		image(x=x, y=y, z=value, useRaster=useRaster, ...)			
#		}
	}
)


setMethod("image", signature(x='RasterStackBrick'), 
	function(x, y=1, maxpixels=100000, useRaster=TRUE, main, ...)  {
		y <- round(y)
		stopifnot(y > 0 & y <= nlayers(x))
		x <- raster(x, y)
		if (missing(main)) {
			main <- names(x)
		}
		image(x, maxpixels=maxpixels, useRaster=useRaster, main=main, ...)
	}	
)

