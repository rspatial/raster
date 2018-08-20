# Author: Robert J. Hijmans
# Date :  March  2009, August 2012
# Licence GPL v3
# updated November 2011
# version 1.0


fourCellsFromXY <- function(object, xy, duplicates=TRUE) {
# if duplicates is TRUE, the same cell number can be returned 
# twice (if point in the middle of division between two cells) or
# four times (if point in center of cell)
	r <- raster(object) # use small object
	stopifnot(is.matrix(xy))
	return( .doFourCellsFromXY(r@ncols, r@nrows, xmin(r), xmax(r), ymin(r), ymax(r), xy, duplicates, .isGlobalLonLat(r)))
}
