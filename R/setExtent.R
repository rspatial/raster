# Author: Robert J. Hijmans
# Date : January 2009
# Version 1.0
# Licence GPL v3

'extent<-' <- function(x, value) {
	return(setExtent(x, value))
}


setExtent <- function(x, ext, keepres=FALSE, snap=FALSE) {
	
#	oldbb <- extent(x)
	bb <- extent(ext)
	if (snap) {
		bb <- alignExtent(bb, x)
	}

	if (inherits(x, 'RasterStack')) {
		if (keepres) {
			stop('you cannot use keepres=TRUE with a RasterStack')
		}
		x@extent <- bb
		if (nlayers(x) > 0) {
			for (i in 1:nlayers(x)) {
				x@layers[[i]]@extent <- bb
			}
		} 
		return(x)
	}

	
	if (keepres) {
		
		newobj <- clearValues(x)
		xrs <- xres(newobj)
		yrs <- yres(newobj)
		newobj@extent <- bb
		nc <- as.integer(round( (newobj@extent@xmax - newobj@extent@xmin) / xrs ))
		if (nc < 1) {
			stop( "xmin and xmax are less than one cell apart" ) 
		} else { 
			newobj@ncols <- nc 
		}
		nr <- as.integer(round( (newobj@extent@ymax - newobj@extent@ymin) / yrs ) )
		if (nr < 1) { 
			stop( "ymin and ymax are less than one cell apart" )
		} else { 
			newobj@nrows <- nr 
		}
		newobj@extent@xmax <- newobj@extent@xmin + newobj@ncols * xrs
		newobj@extent@ymax <- newobj@extent@ymin + newobj@nrows * yrs
		
		if ((x@ncols == newobj@ncols) & (x@nrows == newobj@nrows)) {
			x@extent <- newobj@extent
			return(x)
		} else {
			return(newobj)
		}
		
	} else if (class(x) != "BasicRaster") {
		x@extent <- bb
		return(x)
	}
	
}

