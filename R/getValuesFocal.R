# Author: Robert J. Hijmans
# Date :  March 2012
# Version 1.0
# Licence GPL v3

if (!isGeneric("getValuesFocal")) {
	setGeneric("getValuesFocal", function(x, row, nrows, ngb, ...)
		standardGeneric("getValuesFocal"))
}	

setMethod("getValuesFocal", signature(x='Raster', row='missing', nrows='missing', ngb='numeric'), 
function(x, ngb, names=FALSE, ...) {
	getValuesFocal(x, 1, nrow(x), ngb, names=names, ...)
})


setMethod("getValuesFocal", signature(x='Raster', row='numeric', nrows='numeric', ngb='numeric'), 
function(x, row, nrows, ngb, names=FALSE, padValue=NA, array=FALSE, ...) {

	nl <- nlayers(x)
	if (nl == 0) {
		stop("x has no values")
	} else if (nl > 1) {
		mm <- list()
	}

	xx <- raster(x)
	nc <- ncol(xx)

	row <- round(row)
	nrows <- round(nrows)
	if (!validRow(xx, row)) {
		stop("Not a valid row number")
	}
	if ( (row+nrows-1) > nrow(xx) ) {
		stop("'nrows' is too high")
	}
	stopifnot(is.atomic(padValue))
	geo <- couldBeLonLat(xx)
	
	mask <- FALSE
	if (is.matrix(ngb)) {
		w <- ngb
		ngb <- dim(w)
		w <- ! is.na(as.vector(t(w)))
		mask <- TRUE
	}
	ngb <- .checkngb(ngb, mustBeOdd=TRUE)
	
	ngbr <- floor(ngb[1]/2)
	ngbc <- floor(ngb[2]/2)
	
	
	startrow <- row-ngbr
	endrow <- row+nrows-1+ngbr
	
	sr <- max(1, startrow)  # startrow
	er <- min(endrow, nrow(xx))

	if (nl==1) {
		vv <- matrix(getValues(x, sr, (er-sr+1)), ncol=1)
	} else {
		vv <- getValues(x, sr, (er-sr+1))
	}
	
	for (i in 1:nl) {
		v <- matrix(vv[,i], ncol=nc, byrow=TRUE)
		if (sr > startrow) {
			add <- sr - startrow
			v <- rbind(matrix(padValue, nrow=add, ncol=ncol(v)), v)
		}
		if (endrow > er) {
			add <- endrow - er
			v <- rbind(v, matrix(padValue, nrow=add, ncol=ncol(v)))
		}
		
		if (geo) {
			nv <- ncol(v)
			if (ngbc < nv) {
				v <- cbind(v[,(nv-ngbc+1):nv], v, v[,1:ngbc])
			} else {
				stop('horizontal neighbourhood is too big')
			}
		} else {
			add <- matrix(padValue, ncol=ngbc, nrow=nrow(v))
			v <- cbind(add, v, add)
		}
		
		v <- .Call('_focal_get', as.vector(t(v)), as.integer(dim(v)), as.integer(ngb), NAOK=TRUE, PACKAGE='raster')
		m <- matrix(v, nrow=nrows*nc, byrow=TRUE)
		if (names) {
			rownames(m) <- cellFromRowCol(xx, row, 1):cellFromRowCol(xx, row+nrows-1,nc)
			colnames(m) <- paste('r', rep(1:ngb[1], each=ngb[2]), 'c', rep(1:ngb[2], ngb[1]), sep='')
		}

		if (mask) {
			m <- m[,mask,drop=FALSE]
		}

		if (nl == 1) {
			return(m)
		} else {
			mm[[i]] <- m
		}
	}
	if (array) {
		if (names) {
			dnms <- list(rownames(mm[[1]]), colnames(mm[[1]]), names(x))
		} else {
			dnms <- list(NULL, NULL, names(x))
		}
		mm <- array(unlist(mm, use.names = FALSE), c(nrow(mm[[1]]), ncol(mm[[1]]), length(mm)), dimnames=dnms )
	} else  {
		names(mm) <- names(x)
	}
	return(mm)
}
)


