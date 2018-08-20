# The functions below here were taken from the fields package !!! (image.plot and subroutines)
# to be adjusted for the RasterLayer object.
# fields, Tools for spatial data
# Copyright 2004-2007, Institute for Mathematics Applied Geosciences
# University Corporation for Atmospheric Research
# Licensed under the GPL -- www.gpl.org/licenses/gpl.html

.imageplot <- function (x, y, z, add=FALSE, legend=TRUE, nlevel = 64, horizontal = FALSE, 
# fields, Tools for spatial data
# Copyright 2004-2007, Institute for Mathematics Applied Geosciences
# University Corporation for Atmospheric Research
# Licensed under the GPL -- www.gpl.org/licenses/gpl.html
    legend.shrink = 0.5, legend.width = 0.6, legend.mar = ifelse(horizontal, 3.1, 5.1), legend.lab = NULL, graphics.reset = FALSE, 
    bigplot = NULL, smallplot = NULL, legend.only = FALSE, col = heat.colors(nlevel), 
    lab.breaks = NULL, axis.args = NULL, legend.args = NULL, midpoint = FALSE, box=TRUE, useRaster=FALSE, ...) {

	zlim <- range(z, na.rm = TRUE)

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
	
    if (!legend.only) {
        if (!add) {
            graphics::par(plt = bigplot)
        }
		image(x, y, z, add = add, col = col, useRaster=useRaster, ...)
        big.par <- graphics::par(no.readonly = TRUE)
    } else {
		box <- FALSE
	}

	
	if (legend) {
		if ((smallplot[2] < smallplot[1]) | (smallplot[4] < smallplot[3])) {
			graphics::par(old.par)
			stop("plot region too small to add legend\n")
		}
		ix <- 1
		minz <- zlim[1]
		maxz <- zlim[2]
		binwidth <- (maxz - minz)/nlevel
		midpoints <- seq(minz + binwidth/2, maxz - binwidth/2, by = binwidth)
		iy <- midpoints
		iz <- matrix(iy, nrow = 1, ncol = length(iy))
		breaks <- list(...)$breaks
		graphics::par(new=TRUE, pty = "m", plt=smallplot, err = -1)
		if (!is.null(breaks)) {
			if (is.null(lab.breaks)) {
				lab.breaks <- as.character(breaks)
			}
			axis.args <- c(list(side = ifelse(horizontal, 1, 4), mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2), 
				at = breaks, labels = lab.breaks), axis.args)
		} else {
			axis.args <- c(list(side = ifelse(horizontal, 1, 4), mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2)), axis.args)
		}
		if (!horizontal) {
			if (is.null(breaks)) {
				image(ix, iy, iz, xaxt="n", yaxt="n", xlab="", ylab="", col=col, useRaster=useRaster)
			} else {
				image(ix, iy, iz, xaxt="n", yaxt="n", xlab = "", ylab = "", col=col, breaks=breaks, useRaster=useRaster)
			}
		} else {
			if (is.null(breaks)) {
				image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = col, useRaster=useRaster)
			} else {
				image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = col, breaks = breaks, useRaster=useRaster)
			}
		}
		do.call("axis", axis.args)
		graphics::box()
	
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




.polyimage <- function (x, y, z, col = heat.colors(64), transparent.color = "white", 
# fields, Tools for spatial data
# Copyright 2004-2007, Institute for Mathematics Applied Geosciences
# University Corporation for Atmospheric Research
# Licensed under the GPL -- www.gpl.org/licenses/gpl.html
    midpoint = FALSE, zlim = range(z, na.rm = TRUE), xlim = range(x),  ylim = range(y), add = FALSE, border = NA, ...) {

	polyimageregrid <- function (x) { 
		temp.addcol <- function(X) {
			N <- ncol(X)
			cbind(X[, 1] - (X[, 2] - X[, 1]), X, (X[, N] - X[, (N - 1)]) + X[, N])
		}
		M <- nrow(x)
		N <- ncol(x)
		x <- (x[, 1:(N - 1)] + x[, 2:N])/2
		x <- (x[1:(M - 1), ] + x[2:M, ])/2
		x <- t(temp.addcol(x))
		t(temp.addcol(x))
	}

    drapecolor <- function (z, col = heat.colors(64), zlim = NULL, transparent.color = "white", midpoint = TRUE) {
		eps <- 1e-07
		if (is.null(zlim)) {
			zlim <- range(c(z), na.rm = TRUE)
		}
		z[(z < zlim[1]) | (z > zlim[2])] <- NA
		NC <- length(col)
		M <- nrow(z)
		N <- ncol(z)
		if (midpoint) {
			z <- (z[1:(M - 1), 1:(N - 1)] + z[2:M, 1:(N - 1)] + z[1:(M - 1), 2:N] + z[2:M, 2:N])/4
		}
		dz <- (zlim[2] * (1 + eps) - zlim[1])/NC
		zcol <- floor((z - zlim[1])/dz + 1)
		ifelse(zcol > NC, transparent.color, col[zcol])
	}
	
    Dx <- dim(x)
    Dy <- dim(y)
    if (any((Dx - Dy) != 0)) {
        stop(" x and y matrices should have same dimensions")
    }
    Dz <- dim(z)
    if (all((Dx - Dz) == 0) & !midpoint) {
        x <- polyimageregrid(x)
        y <- polyimageregrid(y)
    }
    zcol <- drapecolor(z, col = col, midpoint = midpoint, zlim = zlim, 
        transparent.color = transparent.color)
    if (!add) {
        plot(xlim, ylim, type = "n", ...)
    }
    N <- ncol(x)
    Nm1 <- N - 1
    M <- nrow(x)
    Mm1 <- M - 1
    for (i in (1:Mm1)) {
        xp <- cbind(x[i, 1:Nm1], x[i + 1, 1:Nm1], x[i + 1, 2:N], 
            x[i, 2:N], rep(NA, Nm1))
        yp <- cbind(y[i, 1:Nm1], y[i + 1, 1:Nm1], y[i + 1, 2:N], 
            y[i, 2:N], rep(NA, Nm1))
        xp <- c(t(xp))
        yp <- c(t(yp))
        graphics::polygon(xp, yp, border = NA, col = c(zcol[i, 1:Nm1]))
    }
}


.imageplotplt <- function (x, add = FALSE, legend.shrink = 0.9, legend.width = 1, 
# fields, Tools for spatial data
# Copyright 2004-2007, Institute for Mathematics Applied Geosciences
# University Corporation for Atmospheric Research
# Licensed under the GPL -- www.gpl.org/licenses/gpl.html
    horizontal = FALSE, legend.mar = NULL, bigplot = NULL, smallplot = NULL, ...) {
    old.par <- graphics::par(no.readonly = TRUE)
    if (is.null(smallplot)) 
        stick <- TRUE
    else stick <- FALSE
    if (is.null(legend.mar)) {
        legend.mar <- ifelse(horizontal, 3.1, 5.1)
    }
    char.size <- ifelse(horizontal, graphics::par()$cin[2]/graphics::par()$din[2], 
        graphics::par()$cin[1]/graphics::par()$din[1])
    offset <- char.size * ifelse(horizontal, graphics::par()$mar[1], graphics::par()$mar[4])
    legend.width <- char.size * legend.width
    legend.mar <- legend.mar * char.size
    if (is.null(smallplot)) {
        smallplot <- old.par$plt
        if (horizontal) {
            smallplot[3] <- legend.mar
            smallplot[4] <- legend.width + smallplot[3]
            pr <- (smallplot[2] - smallplot[1]) * ((1 - legend.shrink)/2)
            smallplot[1] <- smallplot[1] + pr
            smallplot[2] <- smallplot[2] - pr
        }
        else {
            smallplot[2] <- 1 - legend.mar
            smallplot[1] <- smallplot[2] - legend.width
            pr <- (smallplot[4] - smallplot[3]) * ((1 - legend.shrink)/2)
            smallplot[4] <- smallplot[4] - pr
            smallplot[3] <- smallplot[3] + pr
        }
    }
    if (is.null(bigplot)) {
        bigplot <- old.par$plt
        if (!horizontal) {
            bigplot[2] <- min(bigplot[2], smallplot[1] - offset)
        }
        else {
            bottom.space <- old.par$mar[1] * char.size
            bigplot[3] <- smallplot[4] + offset
        }
    }
    if (stick & (!horizontal)) {
        dp <- smallplot[2] - smallplot[1]
        smallplot[1] <- min(bigplot[2] + offset, smallplot[1])
        smallplot[2] <- smallplot[1] + dp
    }
    return(list(smallplot = smallplot, bigplot = bigplot))
}
 


