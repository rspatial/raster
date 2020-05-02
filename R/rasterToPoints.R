# Author: Robert J. Hijmans
# Date :  February 2009
# Version 0.9
# Licence GPL v3


rasterToPoints <- function(x, fun=NULL, spatial=FALSE, ...) {
	
	nl <- nlayers(x)
	if (nl > 1) {
		if (! is.null(fun)) {
			stop('you can only supply a fun argument if "x" has a single layer')		
		}
	}
	
	
	if (! inherits(x, 'RasterStack' )) {
		if ( ! fromDisk(x) & ! inMemory(x) ) {
			if (spatial) {
				return(SpatialPoints(coords=xyFromCell(x, 1:ncell(x)), proj4string=x@crs) )
			} else {
				return(xyFromCell(x, 1:ncell(x)))
			}
		}
	}

	laynam <- names(x)
	
	if (canProcessInMemory(x, 3)) {
		
		xyv <- cbind(xyFromCell(x, 1:ncell(x)), getValues(x))
		if (nl > 1) {
			notna <- apply(xyv[,3:ncol(xyv), drop=FALSE], 1, function(x){ sum(is.na(x)) < length(x) })
			xyv <- xyv[notna, ,drop=FALSE]
		} else {
			xyv <- stats::na.omit(xyv)
			attr(xyv, 'na.action') <- NULL
		}
		if (!is.null(fun)) {
			xyv <- subset(xyv, fun(xyv[,3]))
		}
		
	} else {
	
		xyv <- matrix(NA, ncol=2+nlayers(x), nrow=0)
		colnames(xyv) <- c('x', 'y', names(x))
		X <- xFromCol(x, 1:ncol(x))
		Y <- yFromRow(x, 1:nrow(x))

		tr <- blockSize(x)
		pb <- pbCreate(tr$n, label='rasterize', ...)

		if (nl > 1) {
		
			for (i in 1:tr$n) {
				r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
				xyvr <- cbind(rep(X, tr$nrows[i]), rep(Y[r], each=ncol(x)), getValues(x, row=tr$row[i], nrows=tr$nrows[i]))
				notna <- rowSums(is.na(xyvr[ , 3:ncol(xyvr), drop=FALSE])) < (ncol(xyvr)-2)
				xyvr <- xyvr[notna, ,drop=FALSE]
				xyv <- rbind(xyv, xyvr)
				pbStep(pb, i)
			}
			
		} else {
			# faster
			for (i in 1:tr$n) {
				r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
				v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				xyvr <- cbind(rep(X, tr$nrows[i]), rep(Y[r], each=ncol(x)), v)
				xyvr <- subset(xyvr, !is.na(v))
				if (!is.null(fun)) {
					xyvr <- subset(xyvr, fun(xyvr[,3]))
				}
				xyv <- rbind(xyv, xyvr)
				pbStep(pb, i)
			}
		}
		pbClose(pb)
		
	}
	
	if (spatial) {
		if (nrow(xyv) == 0) {
			xyv <- rbind(xyv, 0)
			v <- data.frame(xyv[ ,-c(1:2), drop=FALSE])
			colnames(v) <- laynam
			s <- SpatialPointsDataFrame(coords=xyv[,1:2,drop=FALSE], data=v, proj4string=x@crs )
			return(s[0,])
		} else {
			v <- data.frame(xyv[ ,-c(1:2), drop=FALSE])
			colnames(v) <- laynam
			return( SpatialPointsDataFrame(coords=xyv[,1:2,drop=FALSE], data=v, proj4string=x@crs ) )
		}
		
	} else {
		colnames(xyv)[3:ncol(xyv)] <- laynam
		return(xyv)
	}
}

