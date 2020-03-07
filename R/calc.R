# Author: Robert J. Hijmans & Matteo Mattiuzzi
# Date :  June 2008
# Version 0.9
# Licence GPL v3




.makeTextFun <- function(fun) {
	if (class(fun)[1] != 'character') {
		if (is.primitive(fun)) {
			test <- try(deparse(fun)[[1]], silent=TRUE)
			if (test == '.Primitive(\"sum\")') { fun <- 'sum' 
			} else if (test == '.Primitive(\"min\")') { fun <- 'min' 
			} else if (test == '.Primitive(\"max\")') { fun <- 'max' 
			}
		} else {
			test1 <- isTRUE(try( deparse(fun)[2] == 'UseMethod(\"mean\")', silent=TRUE))
			test2 <- isTRUE(try( fun@generic == 'mean', silent=TRUE))
			if (test1 | test2) { 
				fun <- 'mean' 
			}
		} 
	}
	return(fun)
}


.getRowFun <- function(fun) {
	if (fun == 'mean') { return(rowMeans)
	} else if (fun == 'sum') { return(rowSums)
	} else if (fun == 'min') { return(.rowMin)
	} else if (fun == 'max') { return(.rowMax)
	} else { stop('unknown fun') }
}



.getColFun <- function(fun) {
	if (fun == 'mean') { return(colMeans)
	} else if (fun == 'sum') { return(colSums)
	} else if (fun == 'min') { return(.colMin)
	} else if (fun == 'max') { return(.colMax)
	} else { stop('unknown fun') }
}



.calcTest <- function(tstdat, fun, na.rm, forcefun=FALSE, forceapply=FALSE) {
	
	if (forcefun & forceapply) {
		forcefun <- FALSE
		forceapply <- FALSE
	}

	
	trans <- FALSE
	doapply <- FALSE
	makemat <- FALSE
	
	nl <- NCOL(tstdat)
		
	if (nl == 1) {

	# the main difference with nl > 1 is that
	# it is important to avoid using apply when a normal fun( ) call will do. 
	# that is a MAJOR time saver. But in the case of a RasterStackBrick it is more
	# natural to try apply first. 	

		if (forceapply) {
			doapply <- TRUE
			makemat <- TRUE	
			tstdat <- matrix(tstdat, ncol=1)
			if (missing(na.rm)) {
				test <- try( apply(tstdat, 1, fun), silent=TRUE)
			} else {
				test <- try( apply(tstdat, 1, fun, na.rm=na.rm), silent=TRUE)
			}
			if (length(test) < length(tstdat) | inherits(test, "try-error")) {
				stop('cannot forceapply this function')
			}
			if (is.matrix(test)) {
				if (ncol(test) > 1) {
					trans <- TRUE
				}
			}
		} else {
			if (! missing(na.rm)) {
				test <- try(fun(tstdat, na.rm=na.rm), silent=TRUE)
				if (inherits(test, "try-error")) {
					test <- try( apply(tstdat, 1, fun, na.rm=na.rm), silent=TRUE)
					doapply <- TRUE
					if (inherits(test, "try-error")) {
						stop("cannot use this function. Perhaps add '...' or 'na.rm' to the function arguments?") 
					} 
					if (is.matrix(test)) {
						if (ncol(test) > 1) {
							trans <- TRUE
						}
					}
				}
			} else {
				test <- try(fun(tstdat), silent=TRUE)
				if (length(test) < length(tstdat) | inherits(test, "try-error")) {
					doapply <- TRUE
					makemat <- TRUE	
					tstdat <- matrix(tstdat, ncol=1)					
					test <- try( apply(tstdat, 1, fun), silent=TRUE)
					if (inherits(test, "try-error")) {
						stop("cannot use this function")
					}
					if (is.matrix(test)) {
						if (ncol(test) > 1) {
							trans <- TRUE
						}
					}
				}
			}
		}

	} else {
	
		if (forcefun) {
			doapply <- FALSE
			test  <- fun(tstdat)
		} else {
			doapply <- TRUE
			if (! missing(na.rm)) {
				test <- try( apply(tstdat, 1, fun, na.rm=na.rm), silent=TRUE)
				if (inherits(test, "try-error")) {
					doapply <- FALSE
					test <- try(fun(tstdat, na.rm=na.rm), silent=TRUE)
					if (inherits(test, "try-error")) {
						stop("cannot use this function. Perhaps add '...' or 'na.rm' to the function arguments?") 
					}
				} else if (is.matrix(test)) {
					trans <- TRUE
				}
			} else {
				test <- try( apply(tstdat, 1, fun), silent=TRUE)
				if (inherits(test, "try-error")) {
					doapply <- FALSE
					test <- try(fun(tstdat), silent=TRUE)
					if (inherits(test, "try-error")) {
						stop("cannot use this function") 
					}
				} else if (is.matrix(test)) {
					trans <- TRUE
				}
			}
		}
	}	
	
	if (trans) {
		test <- t(test)
		test <- ncol(test)
	} else {
		test <- length(test) / 5
	}
	nlout <- as.integer(test)
	
	list(doapply=doapply, makemat=makemat, trans=trans, nlout=nlout)
}

#.calcTest(test[1:5], fun, forceapply=T)


setMethod('calc', signature(x='Raster', fun='function'), 
function(x, fun, filename='', na.rm, forcefun=FALSE, forceapply=FALSE, ...) {

	nl <- nlayers(x)

	test <- .calcTest(x[1:5], fun, na.rm, forcefun, forceapply)
	doapply <- test$doapply
	makemat <- test$makemat
	trans <- test$trans
	nlout <- test$nlout
	if (nlout == 1) {
		out <- raster(x)
	} else {
		out <- brick(x, values=FALSE)
		out@data@nlayers <- nlout
	}

	fun <- .makeTextFun(fun)
	if (class(fun)[1] == 'character') { 
		doapply <- FALSE
		fun <- .getRowFun(fun)
	} 
	
	filename <- trim(filename)
	
	estnl <- (nlayers(x) + nlayers(out)) * 2
	if (canProcessInMemory(x, estnl)) {
		x <- getValues(x)
		if (makemat) { 
			x <- matrix(x, ncol=1) 
		}
		if (missing(na.rm)) {
			if (! doapply ) { 
				x <- fun(x ) 
			} else {
				x <- apply(x, 1, fun )
			}
		} else {
			if ( ! doapply ) { 
				x <- fun(x, na.rm=na.rm ) 
			} else {
				x <- apply(x, 1, fun, na.rm=na.rm)
			}
		}
		if (trans) {
			x <- t(x)
		}
		x <- setValues(out, x)
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return(x)		
	}

# else 
	
	x <- readStart(x)
	out <- writeStart(out, filename=filename, ...)
	tr <- blockSize(out, n=estnl)
	pb <- pbCreate(tr$n, label='calc', ...)			

	if (missing(na.rm)) {
		for (i in 1:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			if ( ! doapply ) {
				v <- fun(v)
				if (nlout > 1 && !is.matrix(v)) {
					v <- matrix(v, ncol=nlout)
				}
			} else {
				if (makemat) { 
					v <- matrix(v, ncol=1) 
				}
				v <- apply(v, 1, fun)
				if (trans) {
					v <- t(v)
				}
			}
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb) 
		}
	} else {
		for (i in 1:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			if ( ! doapply ) {
				v <- fun(v, na.rm=na.rm)
				if (nlout > 1 && !is.matrix(v)) {
					v <- matrix(v, ncol=nlout)
				}
			} else {
				if (makemat) { 
					v <- matrix(v, ncol=1) 
				}
				v <- apply(v, 1, fun, na.rm=na.rm)
				if (trans) {
					v <- t(v)
				}
			}
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb) 
		}
	}
	out <- writeStop(out)
	x <- readStop(x)
	pbClose(pb)
	return(out)
}
)


