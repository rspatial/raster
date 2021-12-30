# The functions is based on a function in the fields package
# Copyright 2004-2007, Institute for Mathematics Applied Geosciences
# University Corporation for Atmospheric Research
# Licensed under the GPL -- www.gpl.org/licenses/gpl.html
#
# Adjustments by Robert Hijmans
# July 2011



.asRaster <- function(x, col, breaks=NULL, r=NULL, colNA=NA, alpha=NULL) {
	if (!is.null(breaks)) {
		if (is.logical(x)) {
			x <- x * 1
		}
		x[] <- as.numeric(cut(as.vector(x), breaks, include.lowest=TRUE))
		
	} else {
		#if (is.function(fun)) {
		#	x[] <- fun(x)
		#}
		if (is.null(r)) {
			r <- range(x, na.rm=TRUE)
		}
		if (r[1] == r[2]) {
			r[1] <- r[1] - 0.001
			r[2] <- r[2] + 0.001
		}
		x <- (x - r[1])/ (r[2] - r[1])
		x <- round(x * (length(col)-1) + 1)
	}
	x[] <- col[x]
	if (!is.na(colNA)) {
		x[is.na(x)] <- grDevices::rgb(t(grDevices::col2rgb(colNA)), maxColorValue=255)
	}
	if (!is.null(alpha)) {
		x[] <- paste(substr(as.vector(x), 1, 7), t(alpha), sep='')
	}
	as.raster(x)
}
	

.rasterImagePlot <- function(x, col, add=FALSE, legend=TRUE, horizontal = FALSE, 
    legend.shrink=0.5, legend.width=0.6, legend.mar = ifelse(horizontal, 3.1, 5.1),
	legend.lab=NULL, graphics.reset=FALSE, bigplot = NULL, smallplot = NULL, legend.only = FALSE, 
    lab.breaks=NULL, axis.args=NULL, legend.args = NULL, interpolate=FALSE, box=TRUE, breaks=NULL, 
	zlim=NULL, zlimcol=NULL, fun=NULL, asp, colNA = NA, alpha=NULL, npretty=0, ...) {

	
	if (!is.null(alpha)) {
			if (is.vector(alpha)) {
				alpha <- matrix(alpha, nrow=nrow(x), ncol=ncol(x), byrow=TRUE)
			}
			alpha <- as.matrix(alpha)
			alpha[alpha < 0] <- 0
			alpha[alpha > 1] <- 1
			alpha[is.na(alpha)] <- 1
			alpha <- alpha * 255 + 1
			a <- c(0:9, LETTERS[1:6])
			a <- paste(rep(a, each=16), rep(a, times=16), sep='')
			a <- a[alpha]
			alpha <- matrix(a, nrow(alpha), ncol(alpha), byrow=TRUE) 
	}
		
	
	ffun <- NULL
	if (is.character(fun)) {
		if (fun %in% c('sqrt', 'log')) {
			if (fun == 'sqrt') {
				ffun <- fun
				fun <- sqrt
			} else {
				ffun <- fun
				fun <- log
			}
		} else {
			fun <- NULL
		}
	} else {
		fun <- NULL
	}
	
	
	lonlat <- .couldBeLonLat(x, warnings=FALSE)
 	if (missing(asp)) {
		if (lonlat) {
			ym <- mean(c(x@extent@ymax, x@extent@ymin))
			asp <- 1/cos((ym * pi)/180)
		} else {
			asp <- 1
		}		
	}
	
	e <- as.vector(t(bbox(extent(x))))
	x <- as.matrix(x)
	if (!is.null(fun)) {
		x <- fun(x)
	}
	x[is.infinite(x)] <- NA
	if (!is.null(zlim)) {
		if (!is.null(zlimcol)) {
			x[x < zlim[1]] <- zlim[1]
			x[x > zlim[2]] <- zlim[2]
		} else { #if (is.na(zlimcol)) {
			x[x < zlim[1] | x > zlim[2]] <- NA
		} 
	}
	
	if (is.null(breaks)) {
		suppressWarnings(zrange <- range(x, zlim, na.rm=TRUE))
	} else {
		suppressWarnings(zrange <- range(x, zlim, breaks, na.rm=TRUE))
	}
	if (! is.finite(zrange[1])) {
		legend <- FALSE 
	} else {
		x <- .asRaster(x, col, breaks, zrange, colNA, alpha=alpha)		
	}
	
    old.par <- graphics::par(no.readonly = TRUE)
    if (add) {
        big.plot <- old.par$plt
    }
    if (legend.only) {
        graphics.reset <- TRUE
    }
	
    if (is.null(legend.mar)) {
        legend.mar <- ifelse(horizontal, 3.1, 5.1)
    }
    temp <- .imageplotplt(add = add, legend.shrink = legend.shrink, legend.width = legend.width, legend.mar = legend.mar, 
									horizontal = horizontal, bigplot = bigplot, smallplot = smallplot)
		
    smallplot <- temp$smallplot
    bigplot <- temp$bigplot


    if (legend.only) {
		box <- FALSE
	} else {
        if (!add) {
            graphics::par(plt = bigplot)
			if (lonlat & (npretty > 0)) {
				lX <- pretty(e[1]:e[2], npretty)	
				lX <- lX[lX >= -180 & lX <= 180]
				lY <- pretty(e[3]:e[4], npretty)
				lY <- lY[lY >= -90 & lY <= 90]
				labelsX <- parse(text=paste(lX, "^o", sep=""))
				labelsY <- parse(text=paste(lY, "^o", sep=""))
				plot(NA, NA, xlim=e[1:2], ylim=e[3:4], type = "n", , xaxs ='i', yaxs = 'i', asp=asp, axes = FALSE, ...)
				graphics::axis(1, lX, labels=labelsX)
				graphics::axis(2, lY, labels=labelsY)
			} else {
				plot(NA, NA, xlim=e[1:2], ylim=e[3:4], type = "n", , xaxs ='i', yaxs = 'i', asp=asp, ...)
			}
		}	
		graphics::rasterImage(x, e[1], e[3], e[2], e[4], interpolate=interpolate)
		big.par <- graphics::par(no.readonly = TRUE)
    } 
	
	if (legend) {
		if ((smallplot[2] < smallplot[1]) | (smallplot[4] < smallplot[3])) {
			graphics::par(old.par)
			stop("plot region is too small. Cannot add a legend\n")
		}
		ix <- 1
		minz <- zrange[1]
		maxz <- zrange[2]
		if (minz == maxz) {
			if (!is.null(breaks)) {
				breaks=minz
			} else {
				minz <- minz - 0.001
				maxz <- maxz + 0.001
			}
		}


		graphics::par(new=TRUE, pty = "m", plt=smallplot, err = -1)
		
		if (!is.null(breaks)) {
			binwidth <- (maxz - minz)/100
			midpoints <- seq(minz, maxz, by = binwidth)
			axis.args <- c(list(side=ifelse(horizontal,1,4), mgp=c(3,1,0), las=ifelse(horizontal,0,2)), axis.args)
			if (is.null(axis.args$at)) {
				axis.args$at <- breaks
			}
			if (is.null(axis.args$labels) ) {
				axis.args$labels=lab.breaks
			}
							
		} else {
			axis.args <- c(list(side = ifelse(horizontal, 1, 4), mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2)), axis.args)
		}
		
		if (!horizontal) {
			plot(NA, NA, xlim=c(0, 1), ylim=c(minz, maxz), type="n", xlab="", ylab="", xaxs ='i', yaxs = 'i', axes=FALSE)
			
			if (is.null(breaks)) {
				mult <- round(max(1, 100 / length(col) ))
				xx <- .asRaster( ((mult*length(col)):1)/mult, col, colNA=colNA) 
			} else {
				xx <- rev(.asRaster(midpoints, col, breaks=breaks, colNA=colNA))
			}

			graphics::rasterImage(xx, 0, minz, 1, maxz, interpolate=FALSE)
			if (!is.null(ffun)) {
				at <- graphics::axTicks(2)
				axis.args$at <- at
				if (ffun=='sqrt') {
					at <- at^2
					if (max(at) > 5) {
						at <- round(at, 0)
					} else {
						at <- round(at, 1)
					}
					at <- unique(at)
					axis.args$at <- sqrt(at)
				} else {
					at <- exp(at)
					if (max(at) > 5) {
						at <- round(at, 0)
					} else {
						at <- round(at, 1)
					}
					at <- unique(at)
					axis.args$at <- log(at)
				}
				axis.args$labels <- at
			}
			do.call(graphics::axis, axis.args)
			graphics::box()
		} else {
			plot(NA, NA, ylim=c(0, 1), xlim=c(minz, maxz), type="n", xlab="", ylab="", xaxs ='i', yaxs = 'i', axes=FALSE)
			
			if (is.null(breaks)) {
				mult <- round(max(1, 100 / length(col) ))
				xx <- t(.asRaster((1:(mult*length(col)))/mult, col, colNA=colNA ))
			} else {
				xx <- t(.asRaster(midpoints, col, breaks=breaks, colNA=colNA))
			}
			graphics::rasterImage(xx, minz, 0, maxz, 1, interpolate=FALSE)
			do.call("axis", axis.args)
			graphics::box()
		}
	
		if (!is.null(legend.lab)) {
			legend.args <- list(text = legend.lab, side = ifelse(horizontal, 1, 4), line = legend.mar - 2)
		}
		if (!is.null(legend.args)) {
			do.call(graphics::mtext, legend.args)
		}
	}
	
	mfg.save <- graphics::par()$mfg
	if (graphics.reset | add) {
		graphics::par(old.par)
		graphics::par(mfg = mfg.save, new = FALSE)
	} else {
		graphics::par(big.par)
		graphics::par(plt = big.par$plt, xpd = FALSE)
		graphics::par(mfg = mfg.save, new = FALSE)
	}
	if (!add & box ) graphics::box()
	invisible()
	
}

