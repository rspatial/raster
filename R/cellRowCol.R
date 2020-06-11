# Author: Robert J. Hijmans
# Date :  October 2008
# Version 0.9
# Licence GPL v3


setMethod(rowFromCell, signature(object="BasicRaster", cell="numeric"), 	
	function(object, cell) {
		object <- raster(object)
		cell <- round(cell)
		cell[cell < 1 | cell > ncell(object)] <- NA
		trunc((cell-1)/ncol(object)) + 1
	}
)

#rowFromCell <- function(object, cell) {
#	object <- raster(object)
#	cell <- round(cell)
#	cell[cell < 1 | cell > ncell(object)] <- NA
#	trunc((cell-1)/ncol(object)) + 1
#}



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


setMethod(cellFromRowColCombine, signature(object="BasicRaster", row="numeric", col="numeric"), 
	function(object, row, col) {
		# faster without this according to PR #131
		# object <- raster(object)
		row[row < 1 | row > object@nrows] <- NA
		col[col < 1 | col > object@ncols] <- NA
		cols <- rep(col, times=length(row))
		dim(cols) <- c(length(col), length(row))
		cols <- t(cols)
		row <- (row-1) * object@ncols
		cols <- cols + row
		as.vector(t(cols))
	}
)

setMethod(colFromCell, signature(object="BasicRaster", cell="numeric"), 	
	function(object, cell) {
		object <- raster(object)
		cell <- round(cell)
		cell[cell < 1 | cell > ncell(object)] <- NA	
		rownr <- trunc((cell-1)/object@ncols) + 1
		as.integer(cell - ((rownr-1) * object@ncols))
	}
)

#colFromCell <- function(object, cell) {
#	object <- raster(object)
#	cell <- round(cell)
#	cell[cell < 1 | cell > ncell(object)] <- NA	
#	rownr <- trunc((cell-1)/object@ncols) + 1
#	as.integer(cell - ((rownr-1) * object@ncols))
#}

.colFromCell <- function(object, cell) {
	nc <- object@ncols
	rownr <- trunc((cell-1)/nc) + 1
	cell - ((rownr-1) * nc)
}


setMethod(rowColFromCell, signature(object="BasicRaster", cell="numeric"), 
	function(object, cell) {
		object <- raster(object)
		cell <- round(cell)
		cell[cell < 1 | cell > ncell(object)] <- NA
		row <- as.integer(trunc((cell-1)/object@ncols) + 1)
		col <- as.integer(cell - ((row-1) * object@ncols))
		return(cbind(row, col))
	}	
)

#rowColFromCell <- function(object, cell) {
#	object <- raster(object)
#	cell <- round(cell)
#	cell[cell < 1 | cell > ncell(object)] <- NA
#	row <- as.integer(trunc((cell-1)/object@ncols) + 1)
#	col <- as.integer(cell - ((row-1) * object@ncols))
#	return(cbind(row, col))
#}


setMethod(cellFromRowCol, signature(object="BasicRaster", row="numeric", col="numeric"), 
	function(object, row, col, ...) {
		rows <- object@nrows
		cols <- object@ncols
		.doCellFromRowCol(rows, cols, row, col)
	}
)

#cellFromRowCol <- function(object, rownr, colnr) {
#	rows <- object@nrows
#	cols <- object@ncols
#	.doCellFromRowCol(rows, cols, rownr, colnr)
#}




