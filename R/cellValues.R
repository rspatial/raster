# Author: Robert J. Hijmans
# Date : November 2008
# Version 1.0
# Licence GPL v3

	
.cellValues <- function(x, cells, layer, nl, df=FALSE, factors=FALSE) { 

	cells[cells < 1 | cells > ncell(x)] <- NA
	
	if (inherits(x, 'RasterLayer')) {
		if (inMemory(x)) {
			if (length(stats::na.omit(cells)) == 0) {
				if (length(cells) == 0) {
					return(NULL)
				}
				return(cells)
			}  # as.numeric to avoid logical values for backwards compatibility
			result <- as.numeric(x@data@values[cells] )
		} else {
			result <- .readCells(x, cells, 1)
		}
	} else {
	
		nlyrs <- nlayers(x)
		if (missing(layer)) { layer <- 1 }
		layer <- min( max( round(layer), 1), nlyrs)
		if (missing(nl)) { nl <- nlyrs }
		nl <-  min( max( round(nl), 1), nlyrs-layer+1 )
		lyrs <- layer:(layer+nl-1)

	
		if (inherits(x, 'RasterStack')) {
	
			result <- matrix(ncol=nl, nrow=length(cells))
			colnames(result) <- names(x)[lyrs]

			if (length(stats::na.omit(cells)) == 0) {
				return(result)
			}			
			
			if (inMemory(x)) {
				for (i in 1:length(lyrs)) {
					result[,i] <- as.numeric(x@layers[[lyrs[i]]]@data@values[cells] )
				}
			} else {
				for (i in 1:length(lyrs)) {
					result[,i] <- .readCells( x@layers[[lyrs[i]]], cells, 1)
				}
			}
		} else if (inherits(x, 'RasterBrick')) {
		
			if (inMemory(x)) {
				result <- x@data@values[cells, lyrs, drop=FALSE] 
				
			} else if (x@file@driver == 'netcdf') {
				result <- .readBrickCellsNetCDF(x, cells, layer, nl) 
				
			}  else {
				result <- .readCells(x, cells, lyrs) 
			}
			
			if (is.null(dim(result))) { 
				result <- matrix(result, ncol=length(lyrs))
			}
			colnames(result) <- names(x)[lyrs]
		}
	}
	if (df) {
		if (!is.matrix(result)) {
			result <- matrix(result)
			colnames(result) <- names(x)
		}
		result <- data.frame(ID=1:NROW(result), result)
		
		facts <- is.factor(x)[lyrs]
		if (any(facts) & factors) {
			if (ncol(result) == 2) {
				# possibly multiple columns added
				result <- cbind(result[,1,drop=FALSE], factorValues(x, result[,2], layer))
			} else {
				# single columns only
				i <- which(facts)
				for (j in i) {
					result <- .insertColsInDF(result, factorValues(x, result[, j+1], j), j+1)
				}
			}
		}

	}
	result
}	


