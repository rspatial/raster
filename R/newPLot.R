# The functions below here were adapted from the functions in the fields package! (image.plot and subroutines)
# fields, Tools for spatial data
# Copyright 2004-2007, Institute for Mathematics Applied Geosciences
# University Corporation for Atmospheric Research
# Licensed under the GPL -- www.gpl.org/licenses/gpl.html


# Adaptations for the raster package:
# Author: Robert J. Hijmans
# Date :  May 2010
# Version 1.0
# Licence GPL v3


.plotSpace <- function(asp=1, legend.mar = 3.1, legend.width = 0.5, legend.shrink = 0.5) {
	
	pars <- graphics::par()
	char.size <- pars$cin[1] / pars$din[1]
    offset <- char.size * pars$mar[4] 
    legend.width <- char.size * legend.width
    legend.mar <- legend.mar * char.size

	legendPlot <- pars$plt
	legendPlot[2] <- 1 - legend.mar
    legendPlot[1] <- legendPlot[2] - legend.width
    pr <- (legendPlot[4] - legendPlot[3]) * ((1 - legend.shrink)/2)
    legendPlot[4] <- legendPlot[4] - pr
    legendPlot[3] <- legendPlot[3] + pr
	
    bp <- pars$plt
    bp[2] <- min(bp[2], legendPlot[1] - offset)
	aspbp = (bp[4]-bp[3]) / (bp[2]-bp[1])
	adj = aspbp / asp
	if (adj < 1) {
		adjust = (bp[4]-bp[3]) - ((bp[4]-bp[3]) * adj)
	} else {
		adjust = (bp[4]-bp[3]) / adj - ((bp[4]-bp[3]))	
	}
	adjust <- adjust / 2

	bp[3] <- bp[3] + adjust
	bp[4] <- bp[4] - adjust

	dp <- legendPlot[2] - legendPlot[1]
    legendPlot[1] <- min(bp[2] + 0.5 * offset, legendPlot[1])
    legendPlot[2] <- legendPlot[1] + dp
    return(list(legendPlot = legendPlot, mainPlot = bp))
}


.plotLegend <- function(z, col, legend.at='classic', lab.breaks = NULL, axis.args = NULL, legend.lab = NULL, legend.args = NULL, ...) {
		horizontal=FALSE
		ix <- 1
		zlim <- range(z, na.rm = TRUE, finite=TRUE)
		zrange <- zlim[2]-zlim[1]
		if (zrange > 10) { decs <- 0
		} else  if (zrange > 1) { decs <- 1
		} else { decs <- ceiling(abs(log10(zrange)) + 1) } 
		pow <- 10^decs

		minz <- floor(zlim[1] * pow) / pow
		maxz <- ceiling(zlim[2] * pow) / pow
		zrange <- maxz - minz
		
		nlevel = length(col)
		binwidth <- c(0, 1:nlevel * (1/nlevel))
		iy <- minz + zrange * binwidth
#		binwidth <- 1 + (maxz - minz)/nlevel
#		iy <- seq(minz, maxz, by = binwidth)
		iz <- matrix(iy, nrow = 1, ncol = length(iy))
		breaks <- list(...)$breaks
		
	
		if (!is.null(breaks) & !is.null(lab.breaks)) {
			axis.args <- c(list(side = ifelse(horizontal, 1, 4), mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2), at = breaks, labels = lab.breaks), axis.args)			
		} else {
			if (legend.at == 'quantile') {
				z <- z[is.finite(z)]
				at = stats::quantile(z, names=F, na.rm=TRUE)
				axis.args <- c(list(side = ifelse(horizontal, 1, 4), mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2), at=at), axis.args)				
#				at <- c(0, 1:5 * (1/5))
#				at <- minz + zrange * at
			} else {
				at <- graphics::axTicks(2, c(minz, maxz, 4))
			}
			at <- round(at, decs)
			axis.args <- c(list(side = ifelse(horizontal, 1, 4), mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2), at=at), axis.args)						
		}
		
		if (!horizontal) {
			if (is.null(breaks)) {
				image(ix, iy, iz, xaxt="n", yaxt="n", xlab = "", ylab = "", col = col)
			} else {
				image(ix, iy, iz, xaxt="n", yaxt="n", xlab = "", ylab = "", col = col, breaks = breaks)
			}
		} else {
			if (is.null(breaks)) {
				image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = col)
			} else {
				image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = col, breaks = breaks)
			}
		}
		axis.args = c(axis.args, cex.axis=0.75, tcl=-0.15, list(mgp=c(3, 0.4, 0)) )
		do.call("axis", axis.args)
		#graphics::axis(axis.args$side, at=min(iz), las=ifelse(horizontal, 0, 2))
		graphics::box()
	
		# title(main = list(legend.lab, cex=1, font=1))
		if (!is.null(legend.lab)) {
			# graphics::mtext(legend.lab, side=3, line=0.75)
			#legend.args <- list(text = legend.lab, side = ifelse(horizontal, 1, 4), line = legend.mar - 2)
			legend.args <- list(text = legend.lab, side=3, line=0.75)
		}
		if (!is.null(legend.args)) {
			#do.call(graphics::mtext, legend.args)
		}
	}


.plot2 <- function(x, maxpixels=100000, col=rev(terrain.colors(25)), xlab='', ylab='', asp, box=TRUE, add=FALSE, legend=TRUE, legend.at='', ...)  {
		

	if (!add & missing(asp)) {
		if (couldBeLonLat(x)) {
			ym <- mean(x@extent@ymax + x@extent@ymin)
			asp <- min(5, 1/cos((ym * pi)/180))
		} else {
			asp = 1
		}		
	}

	plotArea <- .plotSpace(asp)

	x <- sampleRegular(x, maxpixels, asRaster=TRUE, useGDAL=TRUE)

	xticks <- graphics::axTicks(1, c(xmin(x), xmax(x), 4))
	yticks <- graphics::axTicks(2, c(ymin(x), ymax(x), 4))
	
	if (xres(x) %% 1 == 0) xticks = round(xticks)
	if (yres(x) %% 1 == 0) yticks = round(yticks)

	y <- yFromRow(x, nrow(x):1)
	z <- t((getValues(x, format='matrix'))[nrow(x):1,])
	x <- xFromCol(x,1:ncol(x))

	if (add) { 
		image(x=x, y=y, z=z,  col=col, axes=FALSE, xlab=xlab, ylab=ylab, add=TRUE, ...)
	} else {
		if (legend) {
			graphics::par(pty = "m", plt=plotArea$legendPlot, err = -1)
			.plotLegend(z, col, legend.at=legend.at, ...)
			graphics::par(new=TRUE, plt=plotArea$mainPlot) 
		}
		image(x=x, y=y, z=z,  col=col, axes=FALSE, xlab=xlab, ylab=ylab, asp=asp, ...)
		graphics::axis(1, at=xticks,  cex.axis=0.67, tcl=-0.3, mgp=c(3, 0.25, 0))
		las = ifelse(max(nchar(as.character(yticks)))> 5, 0, 1)
		graphics::axis(2, at=yticks, las = las,  cex.axis=0.67, tcl=-0.3, mgp=c(3, 0.75, 0) )
		#graphics::axis(3, at=xticks, labels=FALSE, lwd.ticks=0)
		#graphics::axis(4, at=yticks, labels=FALSE, lwd.ticks=0)
		if (box) graphics::box()
	}
}

#.plot2(r, legend=T)

# .plot2(r, legend.at='quantile')
# plot(wrld_simpl, add=T)
 


 