# Authors: Robert J. Hijmans and Jacob van Etten
# Date : October 2008
# Version 0.9
# Licence GPL v3


.copyWithProperties <- function(x) {
	if (inherits(x, 'RasterStackBrick')) {
		out <- brick(x, values=FALSE)	
	} else { 
		out <- raster(x)
		out@legend <- x@legend
	} 
	names(out) <- names(x)
	out <- setZ(out, getZ(x))
	fx <- is.factor(x)
	if (isTRUE(any(fx))) {
		out@data@isfactor <- fx
		out@data@attributes <- levels(x)
	}
	out
}

setMethod('crop', signature(x='Raster', y='ANY'), 
function(x, y, filename='', snap='near', datatype=NULL, ...) {

	filename <- trim(filename)

	y <- try ( extent(y), silent=TRUE )
	if (class(y) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}
	methods::validObject(y)
	

	out <- .copyWithProperties(x)	
	leg <- out@legend

	e <- intersect(extent(x), extent(y))
	if (is.null(e)) {
		stop('extents do not overlap')
	}
	e <- alignExtent(e, x, snap=snap)
	out <- setExtent(out, e, keepres=TRUE)
	
	if (! hasValues(x)) {
		return(out)
	}

	col1 <- colFromX(x, xmin(out)+0.5*xres(out))
	col2 <- colFromX(x, xmax(out)-0.5*xres(out))
	row1 <- rowFromY(x, ymax(out)-0.5*yres(out))
	row2 <- rowFromY(x, ymin(out)+0.5*yres(out))
	if (row1==1 & row2==nrow(x) & col1==1 & col2==ncol(x)) {
		return(x)
	}

	nc <- ncol(out)
	
	if (is.null(datatype)) { 
		datatype <- unique(c(dataType(x), 'INT2S'))
		if (length(datatype) > 1) {
			datatype <- .commonDataType(datatype)
		}
	} 
	dataType(out) <- datatype
	
	xx <- out
	xx@ncols <- x@ncols # getValuesBlock might read entire rows and then subset
	if (canProcessInMemory(xx, 4)) { 
		nr <- row2 - row1 + 1
		v <- getValuesBlock(x, row1, nrows=nr, col=col1, ncols=nc)
		out <- setValues(out, v)
		if (filename != "") { 
			out <- writeRaster(out, filename=filename, datatype=datatype, ...)			
		}
	} else { 
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='crop', ...)
		out <- writeStart(out, filename=filename, datatype=datatype, ... )
		x <- readStart(x, ...)
		for (i in 1:tr$n) {
			vv <- getValuesBlock(x, row=tr$row[i]+row1-1, nrows=tr$nrows[i], col1, nc)
			out <- writeValues(out, vv, tr$row[i])
			pbStep(pb, i) 			
		} 
		out <- writeStop(out)
		x <- readStop(x)
		pbClose(pb)
	}

	if (!inherits(out, 'RasterStack')) {
		out@legend <- leg
	}
	return(out)
}
)

