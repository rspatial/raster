# Author: Robert J. Hijmans
# Date: Sept 2009
# Version 0.9
# Licence GPL v3



.plotraster <- function(object, col=rev(terrain.colors(25)), maxpixels=100000, axes=TRUE, xlab='', ylab='', ext=NULL, asp, xlim, ylim, add=FALSE, addfun=NULL, main, ...) {

  	if (missing(asp)) {
		if (couldBeLonLat(object, warnings=FALSE)) {
#			ym <- mean(object@extent@ymax + object@extent@ymin)
#			asp <- min(5, 1/cos((ym * pi)/180))
			asp = NA
		} else {
			asp = 1
		}		
	}

	if (missing(main)) {
		main <- '' #names(object)[1]
	}

	if ( ! inMemory(object) ) { 
		if (  !  fromDisk(object) ) {
			stop('no values associated with this RasterLayer')
		} 
	}

	maxpixels <- max(1, maxpixels)

	if (is.null(ext)) {
		e <- extent(object)
	} else  { 
		e <- ext <- intersect(extent(object), ext) 
	}
	
	
	if (! missing(xlim) | ! missing(ylim )) {
		if (!missing(xlim)) { 
			if (xlim[1] >= xlim[2]) stop('invalid xlim')
			if (xlim[1] < e@xmax) e@xmin <- xlim[1]
			if (xlim[2] > e@xmin) e@xmax <- xlim[2]
		}
		if (!missing(ylim)) { 
			if (ylim[1] >= ylim[2]) stop('invalid ylim')
			if (ylim[1] < e@ymax) e@ymin <- ylim[1]
			if (ylim[2] > e@ymin) e@ymax <- ylim[2]
		}
	}
	
	leg <- object@legend
	object <- sampleRegular(object, size=maxpixels, ext=ext, asRaster=TRUE, useGDAL=TRUE)
	x <- (0:ncol(object)) * xres(object) + xmin(object) 
	y <- (0:nrow(object)) * yres(object) + ymin(object) 		

	if (length(leg@color) > 0) {
		breaks <- leg@values
		object <- cut(object, breaks)
		col <- leg@color
		lab.breaks <- as.character(breaks)
	} 
	
	z <- t(as.matrix(object)[object@nrows:1,])
	if (nrow(z) == 1 | ncol(z) == 1) z <- t(z)
	z[is.infinite(z)] <- NA

	if (length(leg@color) > 0) {
		.imageplot(x, y, z, col=col, axes=axes, xlab=xlab, ylab=ylab, asp=asp, breaks=breaks, lab.breaks=lab.breaks, add=add, main=main, ...)
	} else {
		.imageplot(x, y, z, col=col, axes=axes, xlab=xlab, ylab=ylab, asp=asp, add=add, main=main, ...)	
	}
	
	if (!is.null(addfun)) {
		if (is.function(addfun)) {
			addfun()
		}
	}
	
}


