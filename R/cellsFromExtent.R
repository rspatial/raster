# R function for the raster package
# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3


cellsFromExtent <- function(object, extent, expand=FALSE) {
	object <- raster(object) 
	extent <- alignExtent(extent(extent), object)
	innerBox <- intersect(extent(object), extent)
	if (is.null(innerBox)) { 
		return(NULL) 
	}
	
	srow <- rowFromY(object, innerBox@ymax - 0.5 * yres(object))
	erow <- rowFromY(object, innerBox@ymin + 0.5 * yres(object))
	scol <- colFromX(object, innerBox@xmin + 0.5 * xres(object))
	ecol <- colFromX(object, innerBox@xmax - 0.5 * xres(object))
	
	if (expand) {
		srow <- srow - round((extent@ymax - innerBox@ymax) / yres(object))
		erow <- erow + round((innerBox@ymin - extent@ymin) / yres(object))
		scol <- scol - round((innerBox@xmin - extent@xmin) / xres(object))
		ecol <- ecol + round((extent@xmax - innerBox@xmax) / xres(object))
	}

	return(cellFromRowColCombine(object, srow:erow, scol:ecol))
}


# By Mike Sumner
extentFromCells <- function (object, cells) {
	cells <- stats::na.omit(unique(round(cells)))
	cells <- cells[cells > 0 & cells <= ncell(object)]
	if (length(cells) < 1) {
		stop('no valid cells')
	}
	r <- res(object)
    dx <- r[1] * c(-0.5, 0.5)
    dy <- r[2] * c(-0.5, 0.5)
    extent(range(xFromCell(object, cells)) + dx, range(yFromCell(object, cells)) + dy)
}

