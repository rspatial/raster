# Author: Robert J. Hijmans
# Date :  March  2009
# Licence GPL v3
# updated November 2011
# version 1.0



.bilinearValue <- function(raster, xyCoords, layer, n) {

	#bilinear <- function(xy, x, y, v) {
	#	.doBilinear(xy, x, y, v)
	#}

	r <- raster(raster)
	nls <- nlayers(raster)

	four <- fourCellsFromXY(r, xyCoords, duplicates=FALSE)

	xy4 <- matrix(xyFromCell(r, as.vector(four)), ncol=8)
	x <- rbind(.doSpmin(xy4[,1], xy4[,3]), .doSpmax(xy4[,1], xy4[,3]))
	y <- rbind(.doSpmin(xy4[,5], xy4[,6]), .doSpmax(xy4[,5], xy4[,6]))
	# data.frame is faster than cbind in this case (less copying?)
	xy4 <- data.frame(
		x = c(x[1,], x[1,], x[2,], x[2,]),
		y = c(y[1,], y[2,], y[1,], y[2,])
	)
	cells <- cellFromXY(r, xy4)

	w <- getOption('warn')
	options('warn'=-1)
	row1 <- rowFromCell(r, min(cells, na.rm=TRUE))
	options('warn' = w)
	if (is.na(row1)) {
		if (nls == 1) {
			return(rep(NA, nrow(xyCoords)))
		} else {
			return(matrix(NA, nrow= nrow(xyCoords), ncol=nls))
		}
	}

	nrows <- rowFromCell(r, max(cells, na.rm=TRUE)) - row1 + 1
	offs <- cellFromRowCol(r, row1, 1) - 1
	cells <- cells - offs

	if (nls == 1) {
		vv <- getValues(raster, row1, nrows)
		v <- matrix( vv[cells], ncol=4)

		res <- rep(NA, nrow(v))
		rs <- rowSums(is.na(v))
		i <- rs==3
		if (sum(i) > 0) {
			cells <- cellFromXY(raster, xyCoords[i,]) - offs
			res[i] <- vv[cells]
		}
		i <- rs > 0 & rs < 3
		if (sum(i) > 0) {
			vv <- v[i,,drop=FALSE]
			vv[is.na(vv[,1]),1] <- vv[is.na(vv[,1]),2]
			vv[is.na(vv[,2]),2] <- vv[is.na(vv[,2]),1]
			vv[is.na(vv[,3]),3] <- vv[is.na(vv[,3]),4]
			vv[is.na(vv[,4]),4] <- vv[is.na(vv[,4]),3]
			vmean <- rep(rowMeans(vv, na.rm=TRUE), 4)
			vv[is.na(vv)] <- vmean[is.na(vv)]
#			res[i] <- bilinear(xyCoords[i,1], xyCoords[i,2], x[1,i], x[2,i], y[1,i], y[2,i], vv)
			res[i] <- .doBilinear(xyCoords[i,,drop=FALSE], x[,i,drop=FALSE], y[,i,drop=FALSE], vv)
		}
		i <- rs==0
		if (sum(i) > 0) {
#			res[i] <- bilinear(xyCoords[i,1], xyCoords[i,2], x[1,i], x[2,i], y[1,i], y[2,i], v[i,])
			res[i] <- .doBilinear(xyCoords[i, ,drop=FALSE], x[,i,drop=FALSE], y[,i,drop=FALSE], v[i,,drop=FALSE])
		}
		res

	} else {

		if (missing(layer)) { layer <- 1 }
		if (missing(n)) { n <- (nls-layer+1) }
		lyrs <- layer:(layer+n-1)
		allres <- matrix(ncol=length(lyrs), nrow=nrow(xyCoords))
		colnames(allres) <- names(raster)[lyrs]

		cvv <- getValues(raster, row1, nrows)[, lyrs]
		cv <- cvv[cells,]
		for (j in 1:ncol(cv)) {
			v <- matrix(cv[, j], ncol=4)

			res <- rep(NA, nrow(v))
			rs <- rowSums(is.na(v))
			i <- rs==3
			if (sum(i) > 0) {
				cells <- cellFromXY(raster, xyCoords[i,]) - offs
				res[i] <- cvv[cells, j]
			}
			i <- rs > 0 & rs < 3
			if (sum(i) > 0) {
				vv <- v[i,,drop=FALSE]
				vv[is.na(vv[,1]),1] <- vv[is.na(vv[,1]),2]
				vv[is.na(vv[,2]),2] <- vv[is.na(vv[,2]),1]
				vv[is.na(vv[,3]),3] <- vv[is.na(vv[,3]),4]
				vv[is.na(vv[,4]),4] <- vv[is.na(vv[,4]),3]
				vmean <- rep(rowMeans(vv, na.rm=TRUE), 4)
				vv[is.na(vv)] <- vmean[is.na(vv)]
				res[i] <- .doBilinear(xyCoords[i,,drop=FALSE], x[,i,drop=FALSE], y[,i,drop=FALSE], vv)
			}
			i <- rs==0
			if (sum(i) > 0) {
				res[i] <- .doBilinear(xyCoords[i,,drop=FALSE], x[,i,drop=FALSE], y[,i,drop=FALSE], v[i,,drop=FALSE])
			}

			allres[,j] <- res
		}
		allres
	}
}

