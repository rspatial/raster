# Author: Robert J. Hijmans
# Date: Sept 2009
# Version 0.9
# Licence GPL v3


.plotraster2 <- function(object, col=rev(terrain.colors(250)), maxpixels=100000, xlab='', ylab='', ext=NULL, xlim, ylim, add=FALSE, addfun=NULL, colNA=NA, main, facvar=0, alpha=NULL, ...) {

 	if ( ! hasValues(object) ) { 
		stop('no values associated with this RasterLayer')
	}
	maxpixels <- max(1, maxpixels)
	if (is.null(ext)) {
		ext <- extent(object)
	} else  { 
		ext <- intersect(extent(object), ext) 
	}
	if (!missing(xlim)) { 
		if (xlim[1] >= xlim[2]) stop('invalid xlim')
		if (xlim[1] < ext@xmax) ext@xmin <- xlim[1]
		if (xlim[2] > ext@xmin) ext@xmax <- xlim[2]
	} 
	if (!missing(ylim)) { 
		if (ylim[1] >= ylim[2]) stop('invalid ylim')
		if (ylim[1] < ext@ymax) ext@ymin <- ylim[1]
		if (ylim[2] > ext@ymin) ext@ymax <- ylim[2]
	} 
#	leg <- object@legend
	object <- sampleRegular(object, size=maxpixels, ext=ext, asRaster=TRUE)
	if (!is.null(alpha)) {
		if (inherits(alpha, 'RasterLayer')) {
			alpha <- sampleRegular(alpha, size=maxpixels, ext=ext, asRaster=TRUE)
		}
	}
	
	if (facvar > 0) {
		object <- deratify(object, facvar)		
	}
	
	if (missing(main)) {
		main <- ''
		#main <- names(object)
	}	
	
	.rasterImagePlot(object, col=col, xlab=xlab, ylab=ylab, add=add, colNA=colNA, main=main, alpha=alpha, ...)	
	
	if (!is.null(addfun)) {
		if (is.function(addfun)) {
			addfun()
		}
	}

}


