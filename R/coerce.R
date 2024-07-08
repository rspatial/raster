# Author: Robert J. Hijmans
# Date : October 2008
# Version 0.9
# Licence GPL v3



### from terra
setAs("SpatRaster", "Raster", 
	function(from) {
		if (any(terra::window(from))) {
			stop("you must remove the 'window' from the SpatRaster before coercion to a Raster*")
		}
		b <- sources(from, bands=TRUE)
		nl <- nlyr(from)
		e <- as.vector(ext(from))
		prj <- crs(from) #, proj=TRUE)
		if (nl == 1) {
			if (b$source == "") {
				r <- raster::raster(ncols=ncol(from), nrows=nrow(from), crs=prj,
			          xmn=e[1], xmx=e[2], ymn=e[3], ymx=e[4])
				if (hasValues(from)) {
					raster::values(r) <- values(from)
				}
			} else {
				r <- raster::raster(b$source, band=b$bands)
			}
		} else {
			usid <- unique(b$sid)
			if ((length(usid) == 1) & (b$source[1] != "")) {
				if ((nl == nrow(b)) && (b$bands[1] == 1) && (all(diff(b$bands) == 1))) {
					r <- raster::brick(b$source[1])
					if (nlayers(r) != nl) {
						r <- r[[b$bands]]
					}
				} else {
					r <- raster::stack(b$source[1], bands=b$bands)
				}
			} else if (all(b$source=="")) {
				r <- raster::brick(ncol=ncol(from), nrow=nrow(from), crs=prj,
			          xmn=e[1], xmx=e[2], ymn=e[3], ymx=e[4], nl=nlyr(from))
				if (hasValues(from)) {
					raster::values(r) <- values(from)
				}
			} else {
				x <- raster::raster(ncol=ncol(from), nrow=nrow(from), crs=prj,
			          xmn=e[1], xmx=e[2], ymn=e[3], ymx=e[4])
				r <- list()
				b$layer <- 1:nrow(b)
				for (i in usid) {
					bi <- b[b$sid == usid[i], ,drop=FALSE]
					if (bi$source[1] == "") {
						r[[i]] <- raster::setValues(x, values(from[[ bi$layer ]]))
					} else {
						bands <- bi$bands
						if (length(bands) > 1) {
							r[[i]] <- raster::stack(b$source[i], bands=bands)
						} else {
							r[[i]] <- raster::raster(b$source[i], band=bands)						
						}
					}
				}
				r <- raster::stack(r)
			}
		}

		if (hasValues(from)) {
			so <- scoff(from)
			gain(r) <- so[,1]
			offs(r) <- so[,2]
		}
		
		# things that may be different than the file source
		try(levels(r) <- cats(from), silent=TRUE)
		try(names(r) <- names(from))
		#crs(r) <- crs(from)
		extent(r) <- as.vector(ext(from))
		projection(r) <- crs(from, proj=TRUE)
		r
	}
)



## to terra

.fromRasterLayerBrick <- function(from) {
	 
	if (fromDisk(from)) {
		f <- filename(from)
		if (from@file@driver == "netcdf") {
			v <- attr(from@data, "zvar")
			r <- rast(f, v)	
		} else {
			r <- try(rast(f), silent=TRUE)
			if (inherits(r, "try-error")) {
				r <- rast(from + 0)
				levs <- levels(from)[[1]]
				if (!is.null(levs)) {
					levels(r) <- levs
				}
			}
			if (nbands(from) != nlayers(from)) {
				r <- r[[bandnr(from)]]
			}
		}
		if (from@file@NAchanged) {
			NAflag(r) <- from@file@nodatavalue
		}
		scoff(r) <- cbind(gain(from), offs(from))
		
	} else {
		r <- rast(	nrows=nrow(from), 
					ncols=ncol(from),
					nlyrs=nlayers(from),
					extent=extent(from))
		if (hasValues(from)) {
			values(r) <- values(from)
		}
		levs <- levels(from)[[1]]
		if (!is.null(levs)) {
			if (ncol(levs) == 1) {
				levs <- cbind(value=1:nrow(levs), levs)
			}
			levels(r) <- levs				
		}
	}
	if (.hasSlot(from, "srs")) {
		prj <- from@srs
	} else {
		prj <- .srs_from_sp(from@crs)	
	}
	crs(r, warn=FALSE) <- prj	
	
	#z <- from@z
	#if (length(z) == 1) {
	#	z <- z[[1]]
	#	try(time(r) <- z, silent=TRUE)
	#}
	r
}

.fromRasterStack <- function(from) {
	nl <- nlayers(from)
	ff <- sapply(1:nl, function(i) { filename(from[[i]]) })
	uff <- unique(ff)
	if (length(uff) == 1) {
		if (uff == "") {
			return(.fromRasterLayerBrick(from))
		} else {
			n <- nbands(from[[1]])
			bnr <- sapply(1:nl, function(i) { bandnr(from[[i]]) })
			if ((n == nl) && all(bnr == 1:n)) {
				return( rast(uff) )
			}
		}
	} 
	s <- lapply(1:nlayers(from), function(i) {
		x <- from[[i]]
		.fromRasterLayerBrick(x)
	})
	out <- do.call(c, s)
	g <- gc()
	out
}


setAs("Raster", "SpatRaster", 
	function(from) {
		if (inherits(from, "RasterLayer") || inherits(from, "RasterBrick")) { 
			x <- .fromRasterLayerBrick(from)
		} else {
			x <- .fromRasterStack(from)
		}
		names(x) <- names(from)
		ext(x) <- as.vector(extent(from))
		if (.hasSlot(from, "srs")) {
			prj <- from@srs
		} else {
			prj <- .srs_from_sp(from@crs)	
		}
		crs(x, warn=FALSE) <- prj			
		x
	}
)


# To sp pixel/grid objects	


setAs("Raster", "GridTopology", 
	function(from) {
		rs <- res(from)
		orig <- bbox(from)[,1] + 0.5 * rs
		sp::GridTopology(orig, rs, dim(from)[2:1] )
	}
)

setAs("GridTopology", "RasterLayer",
	function(from) {
		raster(extent(from), nrows=from@cells.dim[2], ncols=from@cells.dim[1])
	}
)


setAs("Raster", "SpatialPixels", 
	function(from) {
		if (rotated(from)) {
			stop("\n Cannot coerce because the object is rotated.\n Either coerce to SpatialPoints* object\n or first use the 'rectify' function")
		}	
		sp <- rasterToPoints(from, fun=NULL, spatial=FALSE)
		
		r <- raster(from)
		sp <- sp::SpatialPoints(sp[,1:2,drop=FALSE],  proj4string= .getCRS(r))
		grd <- as(r, "GridTopology")
		sp::SpatialPixels(points=sp, grid=grd)
	}
)

setAs("Raster", "SpatialPixelsDataFrame", 
	function(from) { 
		if (rotated(from)) {
			stop("\n Cannot coerce because the object is rotated.\n Either coerce to SpatialPoints* object\n or first use the 'rectify' function")
		}	
		v <- rasterToPoints(from, fun=NULL, spatial=FALSE)

		r <- raster(from)
		sp <- sp::SpatialPoints(v[,1:2,drop=FALSE],  proj4string= .getCRS(r))

		grd <- as(r, "GridTopology")
		
		if (ncol(v) > 2) {
			v <- data.frame(v[, 3:ncol(v), drop = FALSE])
			if (any(is.factor(from))) {
				f <- levels(from)
				for (i in 1:length(f)) {
					if (!is.null(f[[i]])) {
						v[,i] <- as.factor(f[[i]][v[,i]])
					}
				}
			}
			sp::SpatialPixelsDataFrame(points=sp, data=v, grid=grd)
		} else {
			warning("object has no values, returning a 'SpatialPixels' object")
			sp::SpatialPixels(points=sp, grid=grd)
		}
	}
)


setAs("Raster", "SpatialGrid", 
	function(from) { 
		if (rotated(from)) {
			stop("\n Cannot coerce because the object is rotated.\n Either coerce to SpatialPoints* from\n or first use the 'rectify' function")
		}	
		r <- raster(from)
		grd <- as(r, "GridTopology")
		sp::SpatialGrid(grd,  proj4string=.getCRS(r))
	}
)

setAs("Raster", "SpatialGridDataFrame", 
	function(from) { 
		if (rotated(from)) {
			stop("\n Cannot coerce because the object is rotated.\n Either coerce to SpatialPoints* from\n or first use the 'rectify' function")
		}	

		r <- raster(from)
		grd <- as(r, "GridTopology")

		if (hasValues(from)) {
			sp <- sp::SpatialGridDataFrame(grd,  proj4string=.getCRS(r), data=as.data.frame(from))
		} else { 
			warning("object has no values, returning a 'SpatialGrid' object")
			sp  <- sp::SpatialGrid(grd,  proj4string=.getCRS(r))
		}
		sp
	}
)


# To sp vector objects	

setAs("Raster", "SpatialPolygons", 
	function(from){ 
		r <- rasterToPolygons(from[[1]])
		as(r, "SpatialPolygons")
	}
)

setAs("Raster", "SpatialPolygonsDataFrame", 
	function(from){ 
		return( rasterToPolygons(from) ) 
	} 
)

setAs("Raster", "SpatialPoints", 
	function(from) { 
		sp::SpatialPoints(rasterToPoints(from, spatial=FALSE)[,1:2],  proj4string=.getCRS(from))
	}
)

setAs("Raster", "SpatialPointsDataFrame", 
	function(from) { 
		rasterToPoints(from, spatial=TRUE)
	}
)


setAs("Extent", "SpatialPolygons", 
	function(from){ 
		p <- rbind(c(from@xmin, from@ymin), c(from@xmin, from@ymax), c(from@xmax, from@ymax), c(from@xmax, from@ymin), c(from@xmin, from@ymin) )
		sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(p)), "1"))) 
	}
)

setAs("Extent", "SpatialLines", 
	function(from){ 
		p <- rbind(c(from@xmin, from@ymin), c(from@xmin, from@ymax), c(from@xmax, from@ymax), c(from@xmax, from@ymin), c(from@xmin, from@ymin) )
		sp::SpatialLines(list(sp::Lines(list(sp::Line(p)), "1"))) 
	}
)


setAs("Extent", "SpatialPoints", 
	function(from){ 
		p <- cbind( x=c( from@xmin, from@xmin, from@xmax, from@xmax), y=c(from@ymin, from@ymax, from@ymin, from@ymax) )
		sp::SpatialPoints(p)
	}
)


# to RasterLayer

setAs("SpatialGrid", "RasterLayer", 
	function(from){ return(raster (from)) }
)

setAs("SpatialPixels", "RasterLayer", 
	function(from){ return(raster (from)) }
)



setAs("SpatialGrid", "BasicRaster", 
	function(from){ 
		to <- methods::new("BasicRaster")
		to@extent <- extent(from)
		crs(to) <- from@proj4string
		dim(to) <- c(from@grid@cells.dim[2], from@grid@cells.dim[1])	
		return(to)
	}
)


setAs("SpatialPixels", "BasicRaster", 
	function(from){ 
		to <- methods::new("BasicRaster")
		to@extent <- extent(from)
		crs(to) <- from@proj4string
		dim(to) <- c(from@grid@cells.dim[2], from@grid@cells.dim[1])	
		return(to)
	}
)



# to RasterStack
setAs("SpatialGrid", "RasterStack",
	function(from){ 
		stack(from)
	}
)

setAs("SpatialPixels", "RasterStack", 
	function(from){
		stack(from)
	}
)


# to RasterBrick

setAs("SpatialGrid", "RasterBrick",
	function(from){ 
		return(brick(from)) 
	}
)


setAs("SpatialPixels", "RasterBrick", 
	function(from){ 
		return(brick(from)) 
	}
)



setAs("STFDF", "RasterBrick", 
	function(from) {
		time <- from@time
		nc <- ncol(from@data)
		r <- raster(from@sp)
		b <- brick(r, nl=length(time) * nc)
		b <- setZ(b, rep(time, nc)) # rep changes some time formats
		names(b) <- paste(rep(colnames(from@data), each=length(time)), as.character(time), sep="")
		# need to improve this for character, factor variables
		m <- as.numeric(as.matrix(from@data))
		setValues(b, m)
	}
)


setAs("STSDF", "RasterBrick", 
	function(from) {
		from <- as(from, "STFDF")
		as(from, "RasterBrick")
	}
)



# Between Raster objects
setAs("RasterStack", "RasterLayer", 
	function(from){ return( raster(from)) }
)

setAs("RasterBrick", "RasterLayer", 
	function(from){ return( raster(from)) }
)

setAs("RasterStack", "RasterBrick", 
	function(from){ return( brick(from)) }
)


setAs("RasterBrick", "RasterStack", 
	function(from){ return( stack(from)) }
)

setAs("RasterLayer", "RasterStack", 
	function(from){ return( stack(from)) }
)

setAs("RasterLayer", "RasterBrick", 
	function(from){ return( brick(from)) }
)

setAs("matrix", "RasterLayer",
	function(from){ return(raster(from)) }
)

setAs("RasterLayer", "matrix",
	function(from){ return( getValues(from, format="matrix")) }
)



# "image" 
.rasterToImage <- function(r) {
   x <- xFromCol(r,1:ncol(r))
   y <- yFromRow(r, nrow(r):1)
   z <- t(as.matrix(r)[nrow(r):1,]) 
   list(x=x, y=y, z=z)
}

	

# spatstat
setAs("im", "RasterLayer", 
	function(from) {
		r <- raster(nrows=from$dim[1], ncols=from$dim[2], xmn=from$xrange[1], xmx=from$xrange[2], ymn=from$yrange[1], ymx=from$yrange[2], crs="")
		r <- setValues(r, from$v)
		flip(r, direction="y")
	}
)

# adehabitat
setAs("asc", "RasterLayer", 
	function(from) {
		d <- t(from[])
		d <- d[nrow(d):1, ]
		type <- attr(from, "type") 
		if (type == "factor") {
			warning("factor type converted to numeric")
		}
		cz <- attr(from, "cellsize")
		xmn <- attr(from, "xll") - 0.5 * cz
		ymn <- attr(from, "yll") - 0.5 * cz
		xmx <- xmn + ncol(d) * cz
		ymx <- ymn + nrow(d) * cz
		e <- extent(xmn, xmx, ymn, ymx)
		d <- raster(d)
		extent(d) = e
		return(d)
	}
)


setAs("RasterLayer", "asc", 
	function(from) {
		asc <- getValues(from, format="matrix")
		asc <- asc[nrow(asc):1, ]
		attr(asc, "cellsize") <- xres(from)
		attr(asc, "xll") <- xmin(from) + 0.5 * xres(from)
		attr(asc, "yll") <- ymin(from) + 0.5 * yres(from)
		attr(asc, "type") <- "numeric"
		class(asc) <- "asc"		
		return(asc)	
	}
)



setAs("kasc", "RasterBrick", 
	function(from) {
		names <- colnames(from)
		cz <- attr(from, "cellsize")
		ncol <- attr(from, "ncol")
		nrow <- attr(from, "nrow")
		xmn <- attr(from, "xll") - 0.5 * cz
		ymn <- attr(from, "yll") - 0.5 * cz
		xmx <- xmn + ncol * cz
		ymx <- ymn + nrow * cz
		e <- extent(xmn, xmx, ymn, ymx)
		b <- brick(e, nrow=nrow, ncol=ncol)
		m = matrix(NA, ncol=ncol(from), nrow=nrow(from))
		for (i in 1:ncol(m)) {
			m[,i] <- as.numeric(from[,i])
		}	
		dim(m) <- dim(from)
		b <- setValues(b, m)
		names(b) <- names
		return(b)
	}
)


setAs("kasc", "RasterStack", 
	function(from) {
		names <- colnames(from)
		cz <- attr(from, "cellsize")
		ncol <- attr(from, "ncol")
		nrow <- attr(from, "nrow")
		xmn <- attr(from, "xll") - 0.5 * cz
		ymn <- attr(from, "yll") - 0.5 * cz
		xmx <- xmn + ncol * cz
		ymx <- ymn + nrow * cz
		e <- extent(xmn, xmx, ymn, ymx)
		r <- raster(e, nrow=nrow, ncol=ncol)
		r <- setValues(r, as.numeric(from[,1]))
		names(r) <- names[1]
		s <- stack(r)
		if (ncol(from) > 1) {
			for (i in 2:ncol(from)) {
				r <- setValues(r, as.numeric(from[,i]))
				names(r) <- names[i]
				s <- addLayer(s, r)
			}	
		}
		return(s)
	}
)


# kernel density estimate (kde) from package ks

setAs("kde", "RasterLayer", 
	function(from) {
		x <- t(from$estimate)
		x <- x[nrow(x):1,]
		raster(x, xmn=min(from$eval.points[[1]]), xmx=max(from$eval.points[[1]]), 
					ymn=min(from$eval.points[[2]]), ymx=max(from$eval.points[[2]]) ) 
	}
)


setAs("grf", "RasterBrick", 
	function(from) {
		x <- from$data
		if (!is.matrix(x)) {
			x <- matrix(x)
		}
		ncell <- nrow(x)
		nl <- ncol(x)
		nc <- nr <- as.integer(sqrt(ncell))
		dim(x) <- c(nr, nc, nl)
		
		x = aperm(x, perm=c(2,1,3))
		b <- brick(x)
		b <- flip(b, "y")
		extent(b) <- extent(as.vector(apply(from$coords, 2, range)))
		b
	}
)


setAs("grf", "RasterLayer", 
	function(from) {
		x <- from$data
		if (is.matrix(x)) {
			x <- x[,1]
		}
		ncell <- length(x)
		nc <- nr <- as.integer(sqrt(ncell))
		dim(x) <- c(nr, nc)
		x <- t(x)[nrow(x):1,]
		r <- raster(x)
		extent(r) <- extent(as.vector(apply(from$coords, 2, range)))
		r
	}
)



# setAs("RasterStackBrick", "big.matrix", 
# function(from, filename="") {
	# b <- big.matrix(ncell(from), nlayers(from), backingfile=filename )
	# names(b) <- colnames(from)
	# op <- options("bigmemory.allow.dimnames")
	# options(bigmemory.allow.dimnames=TRUE)
	# colnames(b) <- names(from)
	# options(bigmemory.allow.dimnames=op)
	# if (canProcessInMemory(from)) {
		# b[]  <- as.matrix(from)
	# } else {
		# nc <- ncol(from)
		# tr <- blockSize(from)
		# for (i in 1:tr$n) {
			# start <- ((tr$row[i]-1) * nc) + 1
			# end <- start + (tr$nrows[i] * nc) - 1
			# b[start:end, ] <- getValues(from, row=tr$row[i], nrows=tr$nrows[i])
		# }
	# }	
	# b
# }


