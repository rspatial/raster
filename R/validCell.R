# Author: Robert J. Hijmans
# Date :  October 2008
# Version 0.9
# Licence GPL v3


validCell <- function(object, cell) {
	cell <- round(cell)
	valid <- rep(FALSE, times=length(cell))
	valid[cell > 0 & cell <= ncell(object)] <- TRUE
	return(valid)
}

validRow <- function(object, rownr) {
	rownr <- round(rownr)
	valid <- rep(FALSE, times=length(rownr))
	valid[rownr > 0 & rownr <= object@nrows] <- TRUE
	return(valid)
}

validCol <- function(object, colnr) {
	colnr <- round(colnr)
	valid <- rep(FALSE, times=length(colnr))
	valid[colnr > 0 & colnr <= object@ncols] <- TRUE
	return(valid)
}
