# Author: Robert J. Hijmans
# Date: December 2009
# Version 0.1
# Licence GPL v3



setMethod('density', signature(x='Raster'), 
	function(x, layer, maxpixels=100000, plot=TRUE, main, ...) {

		if (nlayers(x)==1) {
			d <- sampleRegular(x, maxpixels) #, useGDAL=TRUE)
			x <- density(stats::na.omit(d))
			if (plot) {
				if (missing(main)) {
					main=''
				}
				plot(x, main=main, ...)
				return(invisible(x))
			} else {
				return(x)
			}
		}
		
		if (missing(layer)) {
			y <- 1:nlayers(x)
		} else if (is.character(layer)) {
			y <- match(layer, names(x))
		} else {
			y <- layer
		}
		y <- unique(as.integer(round(y)))
		y <- stats::na.omit(y)
		y <- y[ y >= 1 & y <= nlayers(x) ]
		nl <- length(y)
		if (nl == 0) {stop('no existing layers selected')}
		
		if (nl > 1)	{
			res <- list()
			if (nl > 16) {
				warning('only the first 16 layers are plotted')
				nl <- 16
				y <- y[1:16]
			}
			if (missing(main)) {
				main=names(x) 
			}

			nc <- ceiling(sqrt(nl))
			nr <- ceiling(nl / nc)
			
			
			mfrow <- graphics::par("mfrow")
			spots <- mfrow[1] * mfrow[2]
			if (spots < nl) {
				old.par <- graphics::par(no.readonly = TRUE) 
				on.exit(graphics::par(old.par))
				graphics::par(mfrow=c(nr, nc))
			}
			for (i in 1:length(y)) {	
				r <- raster(x, y[i])
				m <- main[y[i]]
				res[[i]] <- density(r, maxpixels=maxpixels, main=m, plot=plot, ...)
			}		
		} else if (nl==1) {
			if (missing(main)) {
				main <- names(x)[y]
			}
			r <- raster(x, y)
			res <- density(r, maxpixels=maxpixels, main=main, plot=plot, ...)
		}
		if (plot) return(invisible(res))
		else return(res)
	}
)

