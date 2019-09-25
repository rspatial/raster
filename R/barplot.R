# Author: Robert J. Hijmans
# Date :  September 2012
# Version 1.0
# Licence GPL v3



setMethod('barplot', 'RasterLayer', 
	function(height, maxpixels=1000000, digits=0, breaks=NULL, col=rainbow, ...)  {
		
		x <- sampleRegular(height, maxpixels)
		adj <- length(x) / ncell(height)
		if (adj < 1) {
			warning('a sample of ', round(100*adj, 1), '% of the raster cells were used to estimate frequencies')
		}

		if (!is.null(digits)) {
			x <- round(x, digits)
		}
		if (!is.null(breaks)) {
			x <- cut(x, breaks)
		}
		
		x <- table(x) / adj
		if (is.function(col)) {
			col <- col(length(x))
		}
		barplot(x, col=col, ...)
	}
)
