# Author: Robert J. Hijmans
# Date: November 2009, Jan 2016
# Version 1.0
# Licence GPL v3


setMethod('as.integer', signature(x='Raster'), 
function(x, filename='', ...) {
	
	if (nlayers(x) > 1) {
		out <- brick(x, values=FALSE)
	} else {
		out <- raster(x)
	}
	
	
	datatype <- list(...)$datatype

	if (canProcessInMemory(x, 2)){
		
		x <- getValues(x)
		x[] <- as.integer(x)
		out <- setValues(out, x)
		
		if (filename != '') {
			if (is.null(datatype)) {
				out <- writeRaster(out, filename, datatype='INT4S', ...)
			} else {
				out <- writeRaster(out, filename, ...)			
			}
		}
		return(out)
		
	} else {
		if (filename == '') {
			filename <- rasterTmpFile()					
		}
		
		if (is.null(datatype)) {
			out <- writeStart(out, filename=filename, datatype='INT4S', ...)
		} else {
			out <- writeStart(out, filename=filename, ...)
		}
			
		tr <- blockSize(x)
		pb <- pbCreate(tr$n, ...)	
		for (i in 1:tr$n) {
			v <- as.integer( getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i] ) )
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i) 
		} 
		pbClose(pb)			
		out <- writeStop(out)		
		return(out)
	}
}
)
	
	

setMethod('as.logical', signature(x='Raster'), 
function(x, filename='', ...) {
	
	if (nlayers(x) > 1) {
		out <- brick(x, values=FALSE)
	} else {
		out <- raster(x)
	}
	
	datatype <- list(...)$datatype
	
	if (canProcessInMemory(x, 2)){
		
		x <- getValues(x)
		x[] <- as.logical(x)
		out <- setValues(out, x)
		if (filename != '') {
			if (is.null(datatype)) {
				out <- writeRaster(out, filename, datatype='INT2S', ...)
			} else {
				out <- writeRaster(out, filename, ...)			
			}
		}
		return(out)
		
	} else {
		if (filename == '') {
			filename <- rasterTmpFile()					
		}
		
		if (is.null(datatype)) {
			out <- writeStart(out, filename=filename, datatype='INT2S', ...)
		} else {
			out <- writeStart(out, filename=filename, ...)
		}
		tr <- blockSize(x)
		pb <- pbCreate(tr$n, ...)	
		for (i in 1:tr$n) {
			v <- as.logical ( getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i] ) )
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i) 
		} 
		pbClose(pb)			
		out <- writeStop(out)		
		return(out)
	}
}
)
