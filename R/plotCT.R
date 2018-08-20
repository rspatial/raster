# Author: Robert J. Hijmans
# Date :  July 2010
# Version 0.9
# Licence GPL v3


.plotCT <- function(x, maxpixels=500000, ext=NULL, interpolate=FALSE, axes, main, xlab='', ylab='', asp, add=FALSE, addfun=NULL, zlim=NULL, zlimcol=NULL, ...) { 
# plotting with a color table

   if (missing(main)) {
        main <- ''
    }

	sethook <- FALSE
	if (!add) {
		graphics::plot.new()
		if (missing(axes)) {
			axes <- FALSE
		} 
		if (!axes) {
			# if (main != "") { } else {
			old.par <- graphics::par(no.readonly = TRUE) 
			#graphics::par(plt=c(0,1,0,1))
			graphics::par(mar=c(0,0,0,0), xaxs='i',yaxs='i')
			
			sethook <- TRUE
		}	
		if (missing(asp)) {
			if (couldBeLonLat(x)) {
				ym <- mean(c(x@extent@ymax, x@extent@ymin))
				asp <- 1/cos((ym * pi)/180)
			} else {
				asp <- 1
			}		
		}
	}
	coltab <- colortable(x)
	x <- sampleRegular(x, maxpixels, ext=ext, asRaster=TRUE, useGDAL=TRUE)
	z <- getValues(x)

	
	if (!is.null(zlim)) { # not that relevant here, but for consistency....
		if (is.null(zlimcol)) {
			z[ z<zlim[1] ] <- zlim[1]
			z[ z>zlim[2] ] <- zlim[2]
		} else { #if (is.na(zlimcol)) {
			z[z<zlim[1] | z>zlim[2]] <- NA
		} 
	}
	

	if (NCOL(coltab) == 2) {
		# not implemented
		z <- as.numeric(cut(z, coltab[,1]))
		coltab <- as.vector(coltab[,2])
	}
	
	z <- z + 1
	z[is.na(z)] <- 1
	if (! is.null(coltab) ) {
		z <- matrix(coltab[z], nrow=nrow(x), ncol=ncol(x), byrow=T)
		z <- as.raster(z)
	} else {
		z <- matrix(z, nrow=nrow(x), ncol=ncol(x), byrow=T)
		z <- as.raster(z, max=max(z)) #, na.rm=TRUE))
	}

	requireNamespace("grDevices")
	bb <- as.vector(extent(x))


	if (! add) {
		plot(c(bb[1], bb[2]), c(bb[3], bb[4]), type = "n", xlab=xlab, ylab=ylab, asp=asp, axes=axes, main=main, ...)
	}
	graphics::rasterImage(z, bb[1], bb[3], bb[2], bb[4], interpolate=interpolate, ...)
	
	if (!is.null(addfun)) {
		if (is.function(addfun)) {
			addfun()
		}
	}

	if (sethook) {
		setHook("plot.new", function(...) {
			w <- getOption('warn')
			on.exit(options('warn' = w))
			options('warn'=-1) 
		    on.exit(graphics::par(old.par))
			}, 	action="replace")
		setHook("plot.new", function(...) setHook("plot.new", NULL, "replace"))
	}
	
}


