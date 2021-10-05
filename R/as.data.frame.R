# Author: Robert J. Hijmans
# Date : July 2011
# Version 1.0
# Licence GPL v3



.insertColsInDF <- function(x, y, col, combinenames=TRUE) {
	cnames <- NULL
	if (combinenames) {
		if (ncol(y) > 1) {
			cnames <- paste(colnames(x)[col], '_', colnames(y), sep='')
		}
	}
	if (ncol(y) == 1) {
		x[, col] <- y
		return(x)
	} else if (col==1) {
		z <- cbind(y, x[, -1, drop=FALSE])
	} else if (col==ncol(x)) {
		z <- cbind(x[, -ncol(x), drop=FALSE], y)
	} else {
		z <- cbind(x[,1:(col-1), drop=FALSE], y, x[,(col+1):ncol(x), drop=FALSE])
	}
	if (!is.null(cnames)) {
		colnames(z)[col:(col+ncol(y)-1)] <- cnames
	}
	z
}


setMethod('as.data.frame', signature(x='Raster'), 
	function(x, row.names = NULL, optional = FALSE, xy=FALSE, na.rm=FALSE, long=FALSE, ...) {

		if (!canProcessInMemory(x, 4) & na.rm) {
			r <- raster(x)
			ncx <- ncol(r)
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, label='as.data.frame', ...)
			x <- readStart(x)
			v <- NULL
			for (i in 1:tr$n) {
				start <- (tr$row[i]-1) * ncx + 1
				end <- start + tr$nrows[i] * ncx - 1
				vv <- cbind(start:end, getValues(x, row=tr$row[i], nrows=tr$nrows[i]))
				if (xy) {
					vv <- cbind(vv, data.frame(xyFromCell(r, start:end)))
				}
				vv <- stats::na.omit(vv)
				v <- rbind(v, vv)
				pbStep(pb, i) 	
			}
			x <- readStop(x)
		} else {
			v <- getValues(x)
			if (xy) {
				XY <- data.frame(xyFromCell(x, 1:ncell(x)))
				v <- cbind(XY, v)
			}
			if (na.rm) {
				v <- stats::na.omit(cbind(1:ncell(x), v))
			}
		}
		
		v <- as.data.frame(v, row.names=row.names, optional=optional, ...)
		if (na.rm) {
			rownames(v) <- as.character(v[,1])
			v <- v[,-1,drop=FALSE]
		} 
		
		if (nlayers(x) == 1) {
			colnames(v)[ncol(v)] <- names(x)  # for nlayers = 1
		}
				
		i <- is.factor(x)
		if (any(is.factor(x))) {
			if (ncol(v) == 1) {
				v <- data.frame( factorValues(x, v[,1], 1))
		#		j <- which(sapply(v, is.character))
		#		if (length(j) > 0) {
		#			for (jj in j) {
		#				v[, jj] <- as.factor(v[,jj])
		#			}
		#		}
			} else {
				nl <- nlayers(x)
				if (ncol(v) > nl) {
					rnge1 <- 1:(ncol(v)-nl)
					rnge2 <- (ncol(v)-nl+1):ncol(v)
					v <- cbind(v[, rnge1], .insertFacts(x, v[, rnge2, drop=FALSE], 1:nl))
				} else {
					v <- .insertFacts(x, v, 1:nl)
				}
			}
		}
	
		if (long) {
			nc <- (ncol(v) - nlayers(x) + 1):ncol(v)
			times <- getZ(x)
			timevar <- 'Z'
			if (is.null(times)) {
				times <- names(x)
				timevar <- 'layer'
			} 
			v <- stats::reshape(v, direction='long', varying=nc, v.names='value', timevar=timevar, times=times)	
			v[ncol(v)] = NULL  # id column
			rownames(v) <- NULL
			#v$layer <- names(x)[v$layer]
		}
		
		v
	}
)


