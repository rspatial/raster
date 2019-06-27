# Author: Robert J. Hijmans
# Date :  October 2011
# Version 1.0
# Licence GPL v3


.checkngb <- function(ngb, mustBeOdd=FALSE) {
	ngb <- as.integer(round(ngb))
	if (length(ngb) == 1) {
		ngb <- c(ngb, ngb)
	} else if (length(ngb) > 2) {
		stop('ngb should be a single value or two values')
	}
	if (min(ngb) < 1) { stop("ngb should be larger than 1") } 
	if (mustBeOdd) {
		if (any(ngb %% 2 == 0)) {
			stop('neighborhood size must be an odd number')
		}
	}
	return(ngb)
}



.wwarn <- function() {
	if (! isTRUE(options('rasterFocalWarningGiven'))) {
		warning('the computation of the weights matrix has changed in version 2.1-35. The sum of weights is now 1')
		options(rasterFocalWarningGiven=TRUE)
	}
}

.getW <- function(w) {
	if (length(w) == 1) {
		w <- round(w)
		stopifnot(w > 0)
		w <- matrix(1, ncol=w, nrow=w)
		w <- w / sum(w)
		.wwarn()
	} else if (length(w) == 2) {
		w <- round(w)
		w <- matrix(1, ncol=w[1], nrow=w[2])
		w <- w / sum(w)
		.wwarn()
	} 
	if (! is.matrix(w) ) {
		stop('w should be a single number, two numbers, or a matrix')
	} 
	return(w)
}





setMethod('focal', signature(x='RasterLayer'), 
function(x, w, fun, filename='', na.rm=FALSE, pad=FALSE, padValue=NA, NAonly=FALSE, ...) {


	stopifnot(hasValues(x))
	
	# mistakes because of differences with old focal and old focalFilter
	dots <- list(...)
	if (!is.null(dots$filter)) {
		warning('argument "filter" is ignored!')
	}
	if (!is.null(dots$ngb)) {
		warning('argument "ngb" is ignored!')		
	}
	
#	w <- .getW(w)
	stopifnot(is.matrix(w))
	d <- dim(w)
	if (prod(d) == 0) { stop('ncol and nrow of w must be > 0') }
	if (min(d %% 2) == 0) { stop('w must have uneven sides') }	
	
	# to get the weights in the (by row) order for the C routine
	# but keeping nrow and ncol as-is
	w[] <- as.vector(t(w))

	out <- raster(x)
	filename <- trim(filename)
	
	padrows <- FALSE
	if (pad) {
		padrows <- TRUE
	}

	gll <- as.integer(.isGlobalLonLat(out))
	if (gll) {
		pad <- TRUE
	}

#	if (NAonly) {
#		na.rm <- TRUE
#	}
	
	dofun <- TRUE
	domean <- FALSE

	if (missing(fun)) {
		dofun <- FALSE
		domean <- FALSE
	} else {
		fun2 <- .makeTextFun(fun)
		if (is.character(fun2)) {
			if (fun2=='mean') {
				domean <- TRUE
				dofun <- FALSE
			} else if (fun2 == 'sum') {
				dofun <- FALSE
			}
		}
	}
	if (dofun) {
		if (na.rm) {
			runfun <- function(x) as.double( fun(x, na.rm=TRUE) )
		} else {
			runfun <- function(x) as.double( fun(x) )
		}
	}
	NAonly <- as.integer(NAonly)
	narm <- as.integer(na.rm)
	domean <- as.integer(domean)
	
	if (canProcessInMemory(out)) {
		if (pad) {
			# this should be done in C, but for now....
			f <- floor(d / 2)
			v <- as.matrix(x)
			if (padrows) {
				padRows <- matrix(padValue, ncol=ncol(out), nrow=f[1])
				v <- rbind(padRows, v, padRows)
			} 
			if (gll) {
				v <- cbind(v[, (ncol(v)-f[2]+1):ncol(v)], v, v[, 1:f[2]])	
			} else {
				padCols <- matrix(padValue, nrow=nrow(v), ncol=f[2])
				v <- cbind(padCols, v, padCols)
			}
			
			paddim <- as.integer(dim(v))
			if (dofun) {
				v <- .focal_fun(as.vector(t(v)), w, paddim, runfun, NAonly)
			} else {
				v <- .focal_sum( as.vector(t(v)), w, paddim, narm, NAonly, domean)
			}

			v <- matrix(v, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
			if (padrows) {
				v <- v[-c(1:f[1], (nrow(v)-f[1]+1):nrow(v)), -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 
			} else {
				v <- v[, -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 			
			}
			v <- as.vector(t(v))
			
		} else {
		
			if (dofun) {
				v <- .focal_fun(values(x), w, as.integer(dim(out)), runfun, NAonly)
			} else {
				v <- .focal_sum( values(x), w, as.integer(dim(out)), narm, NAonly, domean)
			}
		}
		
		out <- setValues(out, v)
		if (filename  != '') {
			out <- writeRaster(out, filename, ...)
		}
		
	} else {

		out <- writeStart(out, filename,...)
		tr <- blockSize(out, minblocks=3, minrows=3)
		pb <- pbCreate(tr$n, label='focal', ...)

		addr <- floor(nrow(w) / 2)
		addc <- floor(ncol(w) / 2)
		nc <- ncol(out)
		nc1 <- 1:(nc * addc)
		
		if (pad) {
			f <- floor(d / 2)
			v <- getValues(x, row=1, nrows=tr$nrows[1]+addr)
			v <- matrix(v, ncol=ncol(out), byrow=TRUE)
			if (padrows) {
				padRows <- matrix(padValue, ncol=ncol(out), nrow=f[1])
				v <- rbind(padRows, v, padRows)
			}
			if (gll) {
				v <- cbind(v[, (ncol(v)-f[2]+1):ncol(v)], v, v[, 1:f[2]])			
			} else {
				padCols <- matrix(padValue, nrow=nrow(v), ncol=f[2])
				v <- cbind(padCols, v, padCols)
			}
			paddim <- as.integer(dim(v))

			if (dofun) {
				v <- .focal_fun(as.vector(t(v)), w, paddim, runfun, NAonly)
			} else {
				v <- .focal_sum( as.vector(t(v)), w, paddim, narm, NAonly, domean)
			}
			v <- matrix(v, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
			if (padrows) {
				v <- v[-c(1:f[1], (nrow(v)-f[1]+1):nrow(v)), -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 
			} else {
				v <- v[ , -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 			
			}
			v <- as.vector(t(v))
			out <- writeValues(out, v, 1)
			pbStep(pb)
			
			for (i in 2:(tr$n-1)) {
				v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+(2*addr))
				v <- matrix(v, ncol=ncol(out), byrow=TRUE)
				if (padrows) {
					padRows <- matrix(padValue, ncol=ncol(out), nrow=f[1])
					v <- rbind(padRows, v, padRows)
				}
				if (gll) {
					v <- cbind(v[, (ncol(v)-f[2]+1):ncol(v)], v, v[, 1:f[2]])			
				} else {				
					padCols <- matrix(padValue, nrow=nrow(v), ncol=f[2])
					v <- cbind(padCols, v, padCols)
				}
				paddim <- as.integer(dim(v))
				if (dofun) {
					v <- .focal_fun(as.vector(t(v)), w, paddim, runfun, NAonly)
				} else {
					v <- .focal_sum( as.vector(t(v)), w, paddim, narm, NAonly, domean)
				}
				v <- matrix(v, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
				if (padrows) {
					v <- v[-c(1:f[1], (nrow(v)-f[1]+1):nrow(v)), -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 
				} else {
					v <- v[, -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 				
				}
				v <- as.vector(t(v))
				out <- writeValues(out, v[-nc1], tr$row[i])
				pbStep(pb) 
			}
			i <- tr$n
			v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+addr)
			v <- matrix(v, ncol=ncol(out), byrow=TRUE)
			if (padrows) {
				padRows <- matrix(padValue, ncol=ncol(out), nrow=f[1])
				v <- rbind(padRows, v, padRows)
			}
			if (gll) {
				v <- cbind(v[, (ncol(v)-f[2]+1):ncol(v)], v, v[, 1:f[2]])			
			} else {
				padCols <- matrix(padValue, nrow=nrow(v), ncol=f[2])
				v <- cbind(padCols, v, padCols)
			}
			paddim <- as.integer(dim(v))

			if (dofun) {
				v <- .focal_fun(as.vector(t(v)), w, paddim, runfun, NAonly)
			} else {
				v <- .focal_sum( as.vector(t(v)), w, paddim, narm, NAonly, domean)
			}
			v <- matrix(v, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
			if (padrows) {
				v <- v[-c(1:f[1], (nrow(v)-f[1]+1):nrow(v)), -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 
			} else {
				v <- v[, -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 				
			}
			v <- as.vector(t(v))
			
			out <- writeValues(out, v[-nc1], tr$row[i])
			pbStep(pb) 
		
		} else {
		
			v <- getValues(x, row=1, nrows=tr$nrows[1]+addr)
			if (dofun) {
				v <- .focal_fun(v, w, as.integer(c(tr$nrows[1]+addr, nc)), runfun, NAonly)
			} else {
				v <- .focal_sum( v, w, as.integer(c(tr$nrows[1]+addr, nc)), narm, NAonly, domean)
			}
			out <- writeValues(out, v, 1)
			pbStep(pb)
			for (i in 2:(tr$n-1)) {
				v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+(2*addr))
				if (dofun) {
					v <- .focal_fun(v, w, as.integer(c(tr$nrows[i]+(2*addr), nc)), runfun, NAonly)
				} else {
					v <- .focal_sum( v, w, as.integer(c(tr$nrows[i]+(2*addr), nc)), narm, NAonly, domean)
				}
				out <- writeValues(out, v[-nc1], tr$row[i])
				pbStep(pb) 
			}
			i <- tr$n
			v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+addr)
			if (dofun) {
				v <- .focal_fun(v, w, as.integer(c(tr$nrows[i]+addr, nc)), runfun, NAonly)
			} else {
				v <- .focal_sum( v, w, as.integer(c(tr$nrows[i]+addr, nc)), narm, NAonly, domean)
			}
			out <- writeValues(out, v[-nc1], tr$row[i])
			pbStep(pb) 
		}
		out <- writeStop(out)			
		pbClose(pb)	
	}
	return(out)
}
)

