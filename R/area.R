# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.9
# Licence GPL v3



.cellArea <- function(x, r=6378137) {
# currently not used
	dlonR2 <- xres(x) * (pi / 180) * r^2
	lat <- yFromRow(x, 1:nrow(x))
	lat <- cbind(lat, lat)

	dlat <- yres(x) 
	lat[,1] <- lat[,1] + 0.5 * dlat
	lat[,2] <- lat[,2] - 0.5 * dlat
	lat <- sin(lat * (pi / 180) )
	# for one column:
	abs(lat[,2] - lat[,1]) * dlonR2
}


if (!isGeneric("area")) {
	setGeneric("area", function(x, ...)
		standardGeneric("area"))
}	

setMethod('area', signature(x='SpatialPolygons'), 
	function(x, ...) {
		if (couldBeLonLat(x)) {
			if (!isLonLat(x)) {
				warning('assuming that the CRS is longitude/latitude!')
			}
			lonlat = TRUE
		} else {
			lonlat = FALSE
		}
		g <- geom(x)
		.Call('_raster_get_area_polygon', PACKAGE = 'raster', g, lonlat)
	}
)	


setMethod('area', signature(x='RasterLayer'), 
	function(x, filename='', na.rm=FALSE, weights=FALSE, ...) {

		out <- raster(x)	
		if (na.rm) {
			if (! hasValues(x) ) {
				na.rm <- FALSE
				warning("'x' has no values, ignoring 'na.rm=TRUE'")
				rm(x)
			}
		} else {
			rm(x)
		}	
	
		if (! couldBeLonLat(out)) {
			warning('This function is only useful for Raster* objects with a longitude/latitude coordinates')
			ar <- prod(res(out))
			return( init(out, function(x) ar, filename=filename, ...) )
		}
	
		filename <- trim(filename)
		if (!canProcessInMemory(out, 3) & filename == '') {
			filename <- rasterTmpFile()
		}
		
		if (filename == '') {
			v <- matrix(NA, ncol=nrow(out), nrow=ncol(out))
		} else {
			if (weights) {
				outfname = filename
				filename = rasterTmpFile()
			}
			out <- writeStart(out, filename=filename, ...)
		}

		dy <- .geodist(0, 0, 0, yres(out)) 

		y <- yFromRow(out, 1:nrow(out))
		dx <- .geodist(0, y, xres(out), y) 


		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='area', ...)

		for (i in 1:tr$n) {
			r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
			vv <- dx[r] * dy / 1000000
			vv <- rep(vv, each=out@ncols)
			if (na.rm) {
				a <- getValues(x, tr$row[i], tr$nrows[i])
				vv[is.na(a)] <- NA
			}
			if (filename == "") {
				v[,r] <- vv
			} else {
				out <- writeValues(out, vv, tr$row[i])
			}
			pbStep(pb, i)
		}

		pbClose(pb)
		
		if (filename == "") { 
			v <- as.vector(v)
			if (weights) {
				v <- v / sum(v, na.rm=TRUE)
			}
			values(out) <- v
		} else {
			out <- writeStop(out)	
			if (weights) {
				total <- cellStats(out, 'sum')
				out <- calc(out, fun=function(x){x/total}, filename=outfname, ...)
			}
		}
		
		return(out)
	}
)



setMethod('area', signature(x='RasterStackBrick'), 
	function(x, filename='', na.rm=FALSE, weights=FALSE, ...) {


		if (! na.rm) {
			return( area(raster(x), filename=filename, na.rm=FALSE, weights=weights, ...) )
		}	
		
		out <- brick(x, values=FALSE)
		if (! couldBeLonLat(out)) {
			stop('This function is only useful for Raster* objects with a longitude/latitude coordinates')
		}
	
		filename <- trim(filename)
		if (!canProcessInMemory(out) & filename == '') {
			filename <- rasterTmpFile()
		}

		nl <- nlayers(out)
		
		if (filename == '') {
			v <- matrix(NA, ncol=nl, nrow=ncell(out))
		} else {
			if (weights) {
				outfname = filename
				filename = rasterTmpFile()
			}
			out <- writeStart(out, filename=filename, ...)
		}

		dy <- pointDistance(c(0,0),c(0, yres(out) ), lonlat=TRUE)
		y <- yFromRow(out, 1:nrow(out))
		dx <- pointDistance(cbind(0, y), cbind(xres(out), y), lonlat=TRUE)

		if (.doCluster() ) {
			cl <- getCluster()
			on.exit( returnCluster() )
			nodes <- min(nrow(out), length(cl))	
			message('Using cluster with ', nodes, ' nodes')
			utils::flush.console()		
				
			tr <- blockSize(out, minblocks=nodes)
			pb <- pbCreate(tr$n, label='area', ...)

#			clFun <- function(i, tr, dx, dy, out, nl) {
			clFun <- function(i) {
				r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
				vv <- dx[r] * dy / 1000000
				vv <- rep(vv, each=out@ncols)
			
				vv <- matrix(rep(vv, times=nl), ncol=nl)
				a <- getValues(x, tr$row[i], tr$nrows[i])
				vv[is.na(a)] <- NA
				return(vv)
			}
			.sendCall <- eval( parse( text="parallel:::sendCall") )

			parallel::clusterExport(cl, c('tr', 'dx', 'dy', 'out', 'nl'), envir=environment())
			
		    for (i in 1:nodes) {
				.sendCall(cl[[i]], clFun, list(i), tag=i)
			}

			for (i in 1:tr$n) {
				d <- .recvOneData(cl)
				if (! d$value$success ) { 
					print(d)
					stop('cluster error') 
				}

				if (filename == "") {
					r <- tr$row[d$value$tag]:(tr$row[d$value$tag]+tr$nrows[d$value$tag]-1)
					start <- (r[1]-1) * ncol(out) + 1
					end <- r[length(r)] * ncol(out) 
					v[start:end, ] <- d$value$value
				} else {
					out <- writeValues(out, d$value$value, tr$row[d$value$tag])
				}

				if ((nodes + i) <= tr$n) {
#					.sendCall(cl[[d$node]], clFun, list(nodes+i, tr, dx, dy, out, nl), tag=nodes+i)
					.sendCall(cl[[d$node]], clFun, list(nodes+i), tag=nodes+i)
				}
				pbStep(pb, i) 	
			}		
			
		} else {

			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='area', ...)
		
		#rows <- 1
			for (i in 1:tr$n) {
				r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
				vv <- dx[r] * dy / 1000000
				vv <- rep(vv, each=out@ncols)
			
				vv <- matrix(rep(vv, times=nl), ncol=nl)
				a <- getValues(x, tr$row[i], tr$nrows[i])
				vv[is.na(a)] <- NA

				if (filename == "") {
					start <- (r[1]-1) * ncol(out) + 1
					end <- r[length(r)] * ncol(out) 
					v[start:end, ] <- vv
				} else {
					out <- writeValues(out, vv, tr$row[i])
				}
				pbStep(pb, i)
			}
			pbClose(pb)
		}
		
		if (filename == "") { 
			if (weights) {
				total <- colSums(v, na.rm=TRUE)
				v <- t( t(v) / total )
			}
			values(out) <- v
		} else {
			out <- writeStop(out)	
			if (weights) {
				total <- cellStats(out, 'sum')
				out <- calc(out, fun=function(x){x / total}, filename=outfname, ...)
			}
		}
		return(out)
	}
)


