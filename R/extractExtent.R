# Author: Robert J. Hijmans
# Date : October 2010
# Version 1.0
# Licence GPL v3


setMethod('extract', signature(x='Raster', y='Extent'), 
 	function(x, y, cellnumbers=FALSE, fun=NULL, na.rm=FALSE, layer=1, nl, df=FALSE, ...) {

		e <- intersect(extent(x), y)
		e <- alignExtent(e, x)

		if (!is.null(fun)) {
			cellnumbers <- FALSE
		} else if (cellnumbers) {
			cell <- cellsFromExtent(x, e)
			value <- extract(x, cell, layer=layer, nl=nl, df=df)
			if (df) {
				value <- data.frame(cell=cell, value)
			} else {
				value <- cbind(cell=cell, value)
			}
			return(value)
		}
		
		r <- res(x)
		e@xmin <- e@xmin + 0.25 * r[1]
		e@xmax <- e@xmax - 0.25 * r[1]
		e@ymin <- e@ymin + 0.25 * r[2]
		e@ymax <- e@ymax - 0.25 * r[2]
	
		row <- rowFromY(x, e@ymax)
		lastrow <- rowFromY(x, e@ymin)
		nrows <- lastrow-row+1
		col <- colFromX(x, e@xmin)
		lastcol <- colFromX(x, e@xmax)
		ncols <- lastcol-col+1
		
		v <- getValuesBlock(x, row, nrows, col, ncols)  
		
		if (nlayers(x) > 1) {
			if (missing(layer)) {
				layer <- 1
			} else {
				layer <- max(min(nlayers(x), layer), 1)
			}
			if (missing(nl)) {
				nl <- nlayers(x) - layer + 1
			} else {
				nl <- max(min(nlayers(x)-layer+1, nl), 1)
			}
			lyrs <- layer:(layer+nl-1)
			v <- v[ , lyrs, drop=FALSE] 
		} else {
			lyrs <- 1
		}
		
		if (! is.null(fun)) {
			if (is.matrix(v)) {
				ln <- colnames(v)
				v <- apply(v, 2, FUN=fun, na.rm=na.rm)
				names(v) <- ln
			} else {
				v <- fun(v, na.rm=na.rm)
			}
		}

		if (df) {
			v <- data.frame(v)
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(x, v, lyrs))
			} else {
				v <- .insertFacts(x, v, lyrs)
			}
		}
		return(v)
	}
)



