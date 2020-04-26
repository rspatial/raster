
.makeSpPolygons <- function(polys, attr=NULL, crs="", ...) {

		x <- data.frame(geom(polys))
		x$cump <- NULL

		ppp <-  SpPolygons$new()
		x <- split(x, x$object)
		for (i in 1:length(x)) {
			y <- x[[i]]
			pp <- SpPoly$new()
			if ( any(y$hole > 0) ) {
				ym <- y[y$hole < 1, ]
				z <- split(ym, ym$part)
				for (j in 1:length(z)) {
					p <- SpPolyPart$new()
					p$set(z[[j]]$x, z[[j]]$y)
					z[[j]] <- p
				}
				yh <- y[y$hole > 0, ]
				zz <- split(yh, yh$part)
				for (j in 1:length(zz)) {
					id <- zz[[j]]$hole[1]
					z[[id]]$setHole(zz[[j]]$x, zz[[j]]$y)
				}
				for (j in 1:length(z)) {
					pp$addPart(z[[j]])			
				}
				
			} else {
				z <- split(y, y$part)
				for (j in 1:length(z)) {
					p <- SpPolyPart$new()
					p$set(z[[j]]$x, z[[j]]$y)
					pp$addPart(p)
				}
			}
			ppp$addPoly(pp)
		}
		
		if (!is.na(crs)) {
			ppp$crs <- crs
		}
		ppp
}


.fasterize <- function(p, r, values, background = NA, filename="", ...) {
	if (class(p) != "Rcpp_SpPolygons") p <- .makeSpPolygons(p)
	if (missing(values)) values <- 1:p$size()
	out <- raster(r)
	if (canProcessInMemory(out, 4)) { 
		out <- setValues(out, p$rasterize(nrow(r), ncol(r), as.vector(extent(r)), values, background))
		if (filename != "") { 
			out <- writeRaster(out, filename=filename, ...)			
		}
		return(out)
	} else {		
		temp <- out
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='rasterize', ...)
		out <- writeStart(out, filename=filename, ... )
		for (i in 1:tr$n) {
			x <- crop(temp, extent(temp, r1=tr$row[i], r2=tr$row[i]+tr$nrows[i]-1, c1=1, c2=ncol(out)))
			#x <- setValues(x, p$rasterize(nrow(x), ncol(x), as.vector(extent(x)), values, background))
			#out <- writeValues(out, values(x), tr$row[i])
			x <- p$rasterize(nrow(x), ncol(x), as.vector(extent(x)), values, background)
			out <- writeValues(out, x, tr$row[i])
			pbStep(pb, i) 			
		} 
		out <- writeStop(out)
		pbClose(pb)
		return(out)
	}
}


.extractPolygons <- function(x, p) {

	addres <- max(res(x)) * 2
	rr <- raster(x)
	er <- as.vector(extent(x))
	sp <- .makeSpPolygons(p)
	npol <- sp$size()
	res <- list(rep(NA, sp$size()))
	for (i in 1:npol) {
		pp <- sp$subset(i-1)
		ep <- pp$extent$vector
		if (!(ep[1] >= er[2] || ep[2] <= er[1] || ep[3] >= er[4] || ep[4] <= er[3])) {
			rc <- crop(rr, extent(ep)+addres)
			rc <- .fasterize(pp, rc, values=1, background = NA)
			xy <- rasterToPoints(rc)[,-3,drop=FALSE]
			if (length(xy) > 0)  {  # catch holes or very small polygons
				res[[i]] <- .xyValues(x, xy)
			}
		}
	}
	res
}
	
	