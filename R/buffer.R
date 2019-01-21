# Author: Robert J. Hijmans
# Date : September 2009
# Version 0.9
# Licence GPL v3



.pointBuffer <- function(xy, d, lonlat=TRUE, a=6378137, f=1/298.257223563, crs=NA, ... ) {
	
	n <- list(...)$quadsegs
	if (is.null(n)) {
		n <- 360 
	} else {
		n <- n * 4
	}
	
	if (length(d)==1) {
		d <- rep(d, nrow(xy))
	} else if (length(d) != nrow(xy)) {
		# recycling
		dd <- vector(length=nrow(xy))
		dd[] <- d
		d <- dd
	}

	n <- max(5, round(n))
	brng <- 1:n * 360/n

	pols <- list()

	if (lonlat) {
		a = 6378137.0
		f = 1/298.257223563
		for (i in 1:nrow(xy)) {
			p <- cbind(xy[i,1], xy[i,2], brng, d[i])
			
			#r <- .Call("geodesic", as.double(p[,1]), as.double(p[,2]), as.double(p[,3]), as.double(p[,4]), as.double(a), as.double(f), PACKAGE='raster')
			#pols[[i]] <- matrix(r, ncol=3, byrow=TRUE)[, 1:2]
			
			r <- .Call("_raster_dest_point", p, TRUE, a, f, PACKAGE='raster')
			pols[[i]] <- r[,1:2]						
		}
	} else {
		brng <- brng * pi/180
		for (i in 1:nrow(xy)) {
			x <- xy[i,1] + d[i] * cos(brng)
			y <- xy[i,2] + d[i] * sin(brng)
			pols[[i]] <- cbind(x, y)
		}
	}
	
	sp <- do.call(spPolygons, pols)
	crs(sp) <- crs
	sp
}




setMethod('buffer', signature(x='Spatial'), 
function(x, width=1, dissolve=TRUE, ...) {

	if (inherits(x, 'SpatialPoints')) {
		
		if (.couldBeLonLat(x)) {
			if (!isLonLat(x)) {
				warning('crs unknown, assuming lonlat')
			}
			lonlat=TRUE
		} else {
			lonlat = FALSE
		}
		
		pb <- .pointBuffer(xy=coordinates(x), d=width, lonlat=lonlat, crs=crs(x), ...)

		if (dissolve) {
			pb <- aggregate(pb)
		} else if (.hasSlot(x, 'data')) {
			pb <- SpatialPolygonsDataFrame(pb, x@data, match.ID=FALSE)
		}
		return(pb)		
	}
	
	stopifnot(requireNamespace("rgeos"))
	rgeos::gBuffer(x, byid=!dissolve, width=width, ...)
}
)


setMethod('buffer', signature(x='RasterLayer'), 
function(x, width=0, filename='', doEdge=FALSE, ...) {

	stopifnot(width > 0)

	if (doEdge) {
		r <- boundaries(x, classes=FALSE, type='inner', progress=.progress(...)) 
		pts <- try(  rasterToPoints(r, fun=function(z){ z>0 } )[,1:2, drop=FALSE] )
	} else {
		pts <- try(  rasterToPoints(x)[,1:2, drop=FALSE] )
	}
	
	if (class(pts) == "try-error") {
		d <- .distanceRows(x, filename=filename, ...) 
		d <- reclassify(d, rbind(c(-1,width, 1), c(width, Inf, NA)))
		return(d)
	}
	if (nrow(pts) == 0) {
		stop('RasterLayer has no NA cells (for which to compute a distance)')
	}
	out <- raster(x)
	filename <- trim(filename)
	
	if (couldBeLonLat(x)) { 
		longlat=TRUE 
	} else { 
		longlat=FALSE 
	}
	                                                                        
	if (canProcessInMemory(out, 6)) {
		pb <- pbCreate(4, label='buffer', ...)
		v <- values(x)
		i <- is.na(v)
		if (!any(i)) {
			stop('raster has no NA values to compute distance to')
		}
		pbStep(pb)
		xy <- xyFromCell(out, which(i))
		vals <- .Call('_raster_distanceToNearestPoint', xy, pts, longlat, 6378137.0, 1/298.257223563, PACKAGE='raster')
		pbStep(pb)
		
		v[!i] <- 1
		v[i] <- NA^(vals > width)
		out <- setValues(out, v)
	
		pbStep(pb)
		if (filename != '') {
			out <- writeRaster(out, filename=filename, ...)
		}
		pbStep(pb)
		pbClose(pb)
		return(out)
	} 
	
	out <- writeStart(out, filename=filename, ...)
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, label='buffer', ...)
	xy <- cbind(rep(xFromCol(out, 1:ncol(out)), tr$nrows[1]), NA)
	for (i in 1:tr$n) {
		if (i == tr$n) {
			xy <- xy[1:(ncol(out)*tr$nrows[i]), ]
		}
		xy[,2] <- rep(yFromRow(out, tr$row[i]:(tr$row[i]+tr$nrows[i]-1)), each=ncol(out))
		vals <- getValues(x, tr$row[i], tr$nrows[i])
		j <- which(is.na(vals))
		vals[] <- 0
		if (length(j) > 0) {
			vals[j] <- .Call('_raster_distanceToNearestPoint', xy[j,,drop=FALSE], pts, longlat, 6378137.0, 1/298.257223563, PACKAGE='raster')
		}
		vals[vals > width] <- NA
		vals[!is.na(vals)] <- 1
		out <- writeValues(out, vals, tr$row[i])
		pbStep(pb) 	
	}	
	pbClose(pb)
	out <- writeStop(out)
	return(out)
}
)

