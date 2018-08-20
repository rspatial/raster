# Author: Robert J. Hijmans
# Date :  November 2011
# Version 1.0
# Licence GPL v3


clusterR <- function(x, fun, args=NULL, export=NULL, filename='', cl=NULL, m=2, ...) {

	if (is.null(cl)) {
		cl <- getCluster()
		on.exit( returnCluster() )
	}
	if (!is.null(export)) {
		parallel::clusterExport(cl, export)	
	}
	
	.sendCall <- eval( parse( text="parallel:::sendCall") )

	nodes <- length(cl)
	
	out <- raster(x)

	m <- max(1, round(m))
	tr <- blockSize(x, minblocks=nodes*m )
	if (tr$n < nodes) {
		nodes <- tr$n
	}
	
	tr$row2 <- tr$row + tr$nrows - 1
	pb <- pbCreate(tr$n, label='clusterR', ...)

	
	if (!is.null(args)) {
		stopifnot(is.list(args))
		
		clusfun <- function(fun, i) {
			r <- crop(x, extent(out, r1=tr$row[i], r2=tr$row2[i], c1=1, c2=ncol(out)))
			r <- do.call(fun, c(r, args))
			getValues(r)
		}
	
	} else {
	
		clusfun <- function(fun, i) {
			r <- crop(x, extent(out, r1=tr$row[i], r2=tr$row2[i], c1=1, c2=ncol(out)))
			r <- fun(r)
			getValues(r)
		}
	}
	
	for (i in 1:nodes) {
		.sendCall(cl[[i]], clusfun, list(fun, i), tag=i)
	}
 	
	if (canProcessInMemory(x)) {

		for (i in 1:tr$n) {
			pbStep(pb, i)
			d <- .recvOneData(cl)
			if (! d$value$success ) { 
				print(d$value$value)
				stop('cluster error') 
			}

			if (i ==1) {
				nl <- NCOL(d$value$value) 
				if (nl > 1) {
					out <- brick(out, nl=nl)
				}
				res <- matrix(NA, nrow=ncell(out), ncol=nl)
			} 
			
			j <- d$value$tag
			res[cellFromRowCol(out, tr$row[j], 1):cellFromRowCol(out, tr$row2[j], ncol(out)), ] <- d$value$value
			ni <- nodes + i
			if (ni <= tr$n) {
				.sendCall(cl[[d$node]], clusfun, list(fun, ni), tag=ni)
			}
		}
		out <- setValues(out, res)
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		}
		pbClose(pb)
		return(out)
	
	} else {
	
		for (i in 1:tr$n) {
			pbStep(pb, i)
			
			d <- .recvOneData(cl)
			if (! d$value$success ) { stop('cluster error') }

			if (i ==1) {
				nl <- NCOL(d$value$value) 
				if (nl > 1) {
					out <- brick(out, nl=nl)
				}
				out <- writeStart(out, filename=filename, ...)
			} 
			
			out <- writeValues(out, d$value$value, tr$row[d$value$tag])
			ni <- nodes + i
			if (ni <= tr$n) {
				.sendCall(cl[[d$node]], clusfun, list(fun, ni), tag=ni)
			}
		}
		out <- writeStop(out)
		pbClose(pb)
		return(out)
	}
}




.clusterR2 <- function(x, fun, args=NULL, filename='', cl=NULL, m=2, ...) {

	if (is.null(cl)) {
		cl <- getCluster()
		on.exit( returnCluster() )
	}

	nodes <- length(cl)
	
	out <- raster(x)

	m <- max(1, round(m))
	tr <- blockSize(x, minblocks=max(nodes+1, nodes*m))
	nodes <- min(nodes, tr$n-1)
	
	tr$row2 <- tr$row + tr$nrows - 1
	pb <- pbCreate(tr$n, label='clusterR', ...)

	canPiM <- canProcessInMemory(x)
	
	.sendCall <- eval( parse( text="parallel:::sendCall") )
	
	if (!is.null(args)) {
		stopifnot(is.list(args))
		
		if (canPiM) {
			clusfun <- function(fun, i) {
				r <- crop(x, extent(out, r1=tr$row[i], r2=tr$row2[i], c1=1, c2=ncol(out)))
				r <- do.call(fun, c(r, args))
				getValues(r)
			}
		} else {
			clusfun <- function(fun, i) {
				r <- crop(x, extent(out, r1=tr$row[i], r2=tr$row2[i], c1=1, c2=ncol(out)))
				r <- do.call(fun, c(r, args))
				writeValues(out, getValues(r), tr$row[i])
				return(i)
			}
		}
	
	} else {
		
		if (canPiM) {
			clusfun <- function(fun, i) {
				r <- crop(x, extent(out, r1=tr$row[i], r2=tr$row2[i], c1=1, c2=ncol(out)))
				r <- fun(r)
				getValues(r)
			}
		} else {
			clusfun <- function(fun, i) {
				r <- crop(x, extent(out, r1=tr$row[i], r2=tr$row2[i], c1=1, c2=ncol(out)))
				r <- fun(r)
				writeValues(out, getValues(r), tr$row[i])
				return(i)
			}
		}
	}
	
 	
	if (canPiM) {

		for (i in 1:nodes) {
			.sendCall(cl[[i]], clusfun, list(fun, i), tag=i)
		}
		
		for (i in 1:tr$n) {
			pbStep(pb, i)
			d <- .recvOneData(cl)
			if (! d$value$success ) { stop('cluster error') }

			if (i ==1) {
				nl <- NCOL(d$value$value) 
				if (nl > 1) {
					out <- brick(out, nl=nl)
				}
				res <- matrix(NA, nrow=ncell(out), ncol=nl)
			} 
			
			j <- d$value$tag
			res[cellFromRowCol(out, tr$row[j], 1):cellFromRowCol(out, tr$row2[j], ncol(out)), ] <- d$value$value
			ni <- nodes + i
			if (ni <= tr$n) {
				.sendCall(cl[[d$node]], clusfun, list(fun, ni), tag=ni)
			}
		}
		out <- setValues(out, res)
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		}
		pbClose(pb)
		return(out)
	
	} else {
	
		r <- crop(x, extent(out, r1=tr$row[1], r2=tr$row2[1], c1=1, c2=ncol(out)))
		r <- fun(values(r))
		nl <- NCOL(r)
		if (nl > 1) {
			out <- brick(out, nl=nl)
		}
		out <- writeStart(out, filename=filename, ...)
		out <- writeValues(out, r, 1)
		
		for (i in 1:nodes) {
			.sendCall(cl[[i]], clusfun, list(fun, i+1), tag=i+1)
		}

		for (i in 2:tr$n) {
			pbStep(pb, i)
			d <- .recvOneData(cl)
			if (! d$value$success ) { stop('cluster error') }
			
			ni <- nodes + i
			if (ni <= tr$n) {
				.sendCall(cl[[d$node]], clusfun, list(fun, ni), tag=ni)
			}
		}
		
		out <- writeStop(out)
		pbClose(pb)
		return(out)
	}
}

