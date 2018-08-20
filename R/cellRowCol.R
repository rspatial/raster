# Author: Robert J. Hijmans
# Date :  October 2008
# Version 0.9
# Licence GPL v3


	
rowFromCell <- function(object, cell) {
	object <- raster(object)
	cell <- round(cell)
	cell[cell < 1 | cell > ncell(object)] <- NA
	trunc((cell-1)/ncol(object)) + 1
}

.rowFromCell <- function(object, cell) {
	trunc((cell-1)/ncol(object)) + 1
}


cellFromRow <- function(object, rownr) {
	object <- raster(object)
	rownr <- round(rownr)
	#if (length(rownr)==1) {
	#	return(cellFromRowCol(object, rownr, 1):cellFromRowCol(object, rownr, object@ncols))
	#}
	cols <- rep(1:ncol(object), times=length(rownr))
	rows <- rep(rownr, each=ncol(object))	
	cellFromRowCol(object, rows, cols)
}


cellFromCol <- function(object, colnr) {
	object <- raster(object)
	colnr <- round(colnr)
	rows <- rep(1:nrow(object), times=length(colnr))
	cols <- rep(colnr, each=nrow(object))
	return(cellFromRowCol(object, rows, cols))
}


.OLD_cellFromRowColCombine <- function(object, rownr, colnr) {
	object <- raster(object)
	rc <- expand.grid(rownr, colnr)
	return( cellFromRowCol(object, rc[,1], rc[,2]))
}


cellFromRowColCombine <- function(object, rownr, colnr) {
	object <- raster(object)
	rownr[rownr < 1 | rownr > object@nrows] <- NA
	colnr[colnr < 1 | colnr > object@ncols] <- NA
	cols <- rep(colnr, times=length(rownr))
	dim(cols) <- c(length(colnr), length(rownr))
	cols <- t(cols)
	rownr <- (rownr-1) * object@ncols
	cols <- cols + rownr
	as.vector(t(cols))
}


colFromCell <- function(object, cell) {
	object <- raster(object)
	cell <- round(cell)
	cell[cell < 1 | cell > ncell(object)] <- NA	
	rownr <- trunc((cell-1)/object@ncols) + 1
	as.integer(cell - ((rownr-1) * object@ncols))
}

.colFromCell <- function(object, cell) {
	nc <- object@ncols
	rownr <- trunc((cell-1)/nc) + 1
	cell - ((rownr-1) * nc)
}

rowColFromCell <- function(object, cell) {
	object <- raster(object)
	cell <- round(cell)
	cell[cell < 1 | cell > ncell(object)] <- NA
	row <- as.integer(trunc((cell-1)/object@ncols) + 1)
	col <- as.integer(cell - ((row-1) * object@ncols))
	return(cbind(row, col))
}

cellFromRowCol <- function(object, rownr, colnr) {
	rows <- object@nrows
	cols <- object@ncols
	.doCellFromRowCol(rows, cols, rownr, colnr)
}




