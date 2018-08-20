# Author: Robert J. Hijmans
# Date :  April 2009
# Version 0.9
# Licence GPL v3


rasterFromCells <- function(x, cells, values=TRUE) {
	x <- raster(x)
	u <- stats::na.omit(unique(cells))  # now removing NAs 2018-02-22
	u <- u[ u > 0 & u <= ncell(x) ]
	if (length(u) == 0) {
		stop('no valid cells')
	}
	cols <- colFromCell(x, u)
	rows <- rowFromCell(x, u)
	res <- res(x)
	x1 <- xFromCol(x, min(cols)) - 0.5 * res[1]
	x2 <- xFromCol(x, max(cols)) + 0.5 * res[1]
	y1 <- yFromRow(x, max(rows)) - 0.5 * res[2]
	y2 <- yFromRow(x, min(rows)) + 0.5 * res[2]
	e <- extent(x1, x2, y1, y2)
	r <- crop(x, e)
	if (values) {
		r <- setValues(r, cellsFromExtent(x, e))
	}
	return(r)
}

