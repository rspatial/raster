# Author: Robert J. Hijmans
# Date : August 2010
# Version 0.9
# Licence GPL v3




.haloText <- function(x, y=NULL, labels, col='black', hc='white', hw=0.1, ... ) {
# with minor modifications from
#From: Greg Snow <Greg.Snow <at> imail.org>
#Subject: Re: Text Contrast in a Plot
#Newsgroups: gmane.comp.lang.r.general
#Date: 2009-04-24 21:23:25 GMT

	xy <- xy.coords(x,y)
	xo <- hw * strwidth('A')
	yo <- hw * strheight('A')

	theta <- seq(pi/4, 2*pi, length.out=8*hw*10)  
	for (i in theta) {
		text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=hc, ... )
	}
	text(xy$x, xy$y, labels, col=col, ... )
}


setMethod('text', signature(x='RasterLayer'), 
	function(x, labels, digits=0, fun=NULL, halo=FALSE, ...) {
		x <- rasterToPoints(x, fun=fun, spatial=FALSE)
		if (missing(labels)) {
			if (NCOL(x) > 2) {
				labels <- as.character(round(x[,3], digits=digits) )
			} else {
				labels <- 1:NROW(x)
			}
		}
		if (halo) {
			.haloText(x[,1], x[,2], labels, ...)
		} else {
			text(x[,1], x[,2], labels, ...)
		}
	}
)

setMethod('text', signature(x='RasterStackBrick'), 
	function(x, labels, digits=0, fun=NULL, halo=FALSE, ...) {
		if (missing(labels)) {
			labels <- 1
		}
		if (length(labels) != ncell(x)) {
			labels <- labels[1]
			if (is.character(labels)) {
				i <- which(labels == names(x))
				if (i == 0) {
					i <- 1
				} 
			}
			x <- x[[labels]]
			x <- rasterToPoints(x, fun=fun, spatial=FALSE)
			labels <- as.character(round(x[,3], digits=digits) )
		}
		if (halo) {
			.haloText(x[,1], x[,2], labels, ...)
		} else {
			text(x[,1], x[,2], labels, ...)
		}
	}
)


setMethod('text', signature(x='SpatialPolygons'), 
	function(x, labels, halo=FALSE, ...) {
		if (missing(labels)) {
			labels <- 1
		}
		
		if (length(labels)  == 1) {
			if (.hasSlot(x, 'data')) {
				if (labels %in% names(x)) {
					labels <- x@data[, labels]
				}
			} else {
				if (length(x)> 1) {
					labels <- 1:length(x)
				}
			}
			labels <- as.character(labels)
		}
		
		xy <- coordinates(x)
		if (halo) {
			.haloText(xy[,1], xy[,2], labels, ...)
		} else {
			text(xy[,1], xy[,2], labels, ...)
		}
	}
)


setMethod('text', signature(x='SpatialPoints'), 
	function(x, labels, halo=FALSE, ...) {

		if (missing(labels)) {
			labels <- 1
		}
		
		if (length(labels)  == 1) {
			if (.hasSlot(x, 'data')) {
				if (labels %in% names(x)) {
					labels <- x@data[, labels]
				}
			} else {
				if (length(x)> 1) {
					labels <- 1:length(x)
				}
			}
			labels <- as.character(labels)
		}

		xy <- coordinates(x)		
		if (halo) {
			.haloText(xy[,1], xy[,2], labels, ...)
		} else {
			text(xy[,1], xy[,2], labels, ...)
		}
	}
)

