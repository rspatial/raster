# Author: Robert J. Hijmans
# Date : November 2009
# Version 0.9
# Licence GPL v3


setMethod('sampleRegular', signature(x='Raster'), 
function( x, size, ext=NULL, cells=FALSE, xy=FALSE, asRaster=FALSE, sp=FALSE, useGDAL=FALSE, ...) {

	stopifnot(hasValues(x) | isTRUE(xy))
	
	size <- round(size)
	stopifnot(size > 0)
	nl <- nlayers(x)
	rotated <- rotated(x)
	
	if (is.null(ext)) {
		rcut <- raster(x)
		firstrow <- 1
		lastrow <- nrow(rcut)
		firstcol <- 1
		lastcol <- ncol(rcut)
	} else {
		rcut <- crop(raster(x), ext)
		ext <- extent(rcut)
		yr <- yres(rcut)
		xr <- xres(rcut)
		firstrow <- rowFromY(x, ext@ymax-0.5 *yr)
		lastrow <- rowFromY(x, ext@ymin+0.5*yr)
		firstcol <- colFromX(x, ext@xmin+0.5*xr)
		lastcol <- colFromX(x, ext@xmax-0.5*xr)
	}

	allx <- FALSE
	if (size >= ncell(rcut)) {
		if (!is.null(ext)) {
			x <- crop(x, ext)
		}
		if (asRaster & !rotated) {
			return(x)
		}
		
		nr <- nrow(rcut)
		nc <- ncol(rcut)
		allx <- TRUE
		
	} else {
		Y <- X <- sqrt(ncell(rcut)/size)
		nr <- max(1, floor((lastrow - firstrow + 1) / Y))
		nc <- max(1, floor((lastcol - firstcol + 1) / X))

		rows <- (lastrow - firstrow + 1)/nr * 1:nr + firstrow - 1
		rows <- rows - (0.5 * (lastrow - firstrow + 1)/nr)
		cols <- (lastcol - firstcol + 1)/nc * 1:nc  + firstcol - 1
		cols <- cols - (0.5 * (lastcol - firstcol + 1)/nc)

		cols <- unique(round(cols))
		rows <- unique(round(rows))
		cols <- cols[cols > 0]
		rows <- rows[rows > 0]
		nr <- length(rows)
		nc <- length(cols)
	}
	
	hv <- hasValues(x)
	if (fromDisk(x) & useGDAL & hv) {

		if ( any(rotated | .driver(x, FALSE) != 'gdal') ) { 

			useGDAL <- FALSE 
			
		} else {
		
			offs <- c(firstrow,firstcol)-1
			reg <- c(nrow(rcut), ncol(rcut))-1
			
			if ( nl > 1 ) {
				
				v <- matrix(NA, ncol=nl, nrow=prod(nr, nc))
				
				for (i in 1:nl) {
					xx <- x[[i]]
					con <- rgdal::GDAL.open(xx@file@name, silent=TRUE)
					band <- bandnr(xx)
					vv <- rgdal::getRasterData(con, band=band, offset=offs, region.dim=reg, output.dim=c(nr, nc)) 
					rgdal::closeDataset(con)
					if (xx@data@gain != 1 | xx@data@offset != 0) {
						vv <- vv * xx@data@gain + xx@data@offset
					}
					if (xx@file@nodatavalue < 0) {
						vv[vv <= xx@file@nodatavalue] <- NA
					} else {
						vv[vv == xx@file@nodatavalue] <- NA
					}
					v[, i] <- vv
				}
				
			} else {
			
				band <- bandnr(x)
				con <- rgdal::GDAL.open(x@file@name, silent=TRUE)
				v <- rgdal::getRasterData(con, band=band, offset=offs, region.dim=reg, output.dim=c(nr, nc)) 
				rgdal::closeDataset(con)

				v <- matrix(v, ncol=1)
				colnames(v) <- names(x)
		
				if (x@data@gain != 1 | x@data@offset != 0) {
					v <- v * x@data@gain + x@data@offset
				}
				
				if (.naChanged(x)) {
					if (x@file@nodatavalue < 0) {
						v[v <= x@file@nodatavalue] <- NA
					} else {
						v[v == x@file@nodatavalue] <- NA
					}
				}
				
			}
	
			if (asRaster) {
				if (is.null(ext))  {
					outras <- raster(x)
				} else {
					outras <- raster(ext) 
					crs(outras) <- crs(x)
				}
				nrow(outras) <- nr
				ncol(outras) <- nc
				if (nl > 1) {
					outras <- brick(outras, nl=nl)
					outras <- setValues(outras, v)
				} else {
					outras <- setValues(outras, as.vector(v))
				}
				names(outras) <- names(x)
				if (any(is.factor(x))) {
					levels(outras) <- levels(x)
				}
				return(outras)
				
			} else {
				if (cells) {
					warning("'cells=TRUE' is ignored when 'useGDAL=TRUE'")
				}
				if (xy) {
					warning("'xy=TRUE' is ignored when 'useGDAL=TRUE'")
				}
				if (sp) {
					warning("'sp=TRUE' is ignored when 'useGDAL=TRUE'")
				}
				return( v )
			}
		}
	}
	
	if (allx) {
		cell <- 1:ncell(rcut)
	} else {
		cell <- cellFromRowCol(x, rep(rows, each=nc), rep(cols, times=nr))
	}
	
	if (asRaster) {
		if (rotated) {
			if (is.null(ext)) {
				outras <- raster(extent(x))
			} else {
				outras <- raster(ext)
				crs(outras) <- crs(x)
			}
			ncol(outras) <- nc
			nrow(outras) <- nr
			xy <- xyFromCell(outras, 1:ncell(outras))
			if (hv) {
				m <- .xyValues(x, xy)
			} else {
				m <- NA
			}
			
		} else {
			
			if (allx) {
				if (!is.null(ext)) {
					return(crop(x, ext))
				} else {
					return(x)
				}
			} 
			
			
			cell <- cellFromRowCol(x, rep(rows, each=nc), rep(cols, times=nr))
			if (hv) {
				m <- .cellValues(x, cell)
			} else {
				m <- NA
			}

			if (is.null(ext))  {
				outras <- raster(x)
			} else {
				outras <- raster(ext) 
				crs(outras) <- crs(x)
			}
			nrow(outras) <- nr
			ncol(outras) <- nc
			
		}
		if (nl > 1) {
			outras <- brick(outras, nl=nl)
		}
		
		outras <- setValues(outras, m)
		names(outras) <- names(x)
		if (any(is.factor(x))) {
			levels(outras) <- levels(x)
		}
		return(outras)
		
	} else {
		
		if (allx) {
			cell <= 1:ncell(rcut)
		} else {
			cell <- cellFromRowCol(x, rep(rows, each=nc), rep(cols, times=nr))
		}
		m <- NULL
		nstart <- 1
		if (xy) {
			m <- xyFromCell(x, cell)
			nstart <- 3
		}
		if (cells) {
			m <- cbind(m, cell=cell)
			nstart <- nstart + 1
		} 
		if (hv) {
			m <- cbind(m, .cellValues(x, cell))
			colnames(m)[nstart:(nstart+nl-1)] <- names(x)
		} 
		

		if (sp) {
			if (hv) {
				m <- sp::SpatialPointsDataFrame(xyFromCell(x, cell), data.frame(m),  proj4string=.getCRS(x))
			} else {
				m <- sp::SpatialPoints(xyFromCell(x, cell),  proj4string=.getCRS(x))			
			}
		}
		
		return(m)
	}	
}

)
