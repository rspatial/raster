# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod('cover', signature(x='RasterStackBrick', y='Raster'), 
	function(x, y, ..., filename=''){ 

	
	rasters <- .makeRasterList(x, y, ..., unstack=FALSE)
	compareRaster(rasters)
	
	nl <- sapply(rasters, nlayers)
	un <- unique(nl)
	if (length(un) > 3) {
		stop('number of layers does not match')
	} else if (length(un) == 2 & min(un) != 1) {
		stop('number of layers does not match')
	} else if (nl[1] != max(un)) {
		stop('number of layers of the first object must be the highest') 
	}
	
	outRaster <- brick(x, values=FALSE)
	compareRaster(rasters)

	
	filename <- trim(filename)
	dots <- list(...)
	if (is.null(dots$format))  { 
		format <- .filetype(filename=filename)
	} else { 
		format <- dots$format 
	}
	if (is.null(dots$overwrite)) { 
		overwrite <- .overwrite()	
	} else {
		overwrite <- dots$overwrite
	}
	if (is.null(dots$progress)) { 
		progress <- .progress() 
	} else {
		progress <- dots$progress
	}
	if (is.null(dots$datatype)) { 
		datatype <- unique(dataType(x))
		if (length(datatype) > 1) {
			datatype <- .commonDataType(datatype)
		}
	} else {
		datatype <- dots$datatype
	}	
	


	if ( canProcessInMemory(x, sum(nl)+nl[1])) {

		v <- getValues( rasters[[1]] )
		v2 <- v
		for (j in 2:length(rasters)) {
			v2[] <- getValues( rasters[[j]] )
			v[is.na(v)] <- v2[is.na(v)]
		}	
		outRaster <- setValues(outRaster, v)
		if (filename != '') {
			outRaster <- writeRaster(outRaster, filename=filename, format=format, datatype=datatype, overwrite=overwrite, progress=progress )
		}
		
	} else {
	
		if (filename == '') { filename <- rasterTmpFile() }
		outRaster <- writeStart(outRaster, filename=filename, format=format, datatype=datatype, overwrite=overwrite)
		
		tr <- blockSize(outRaster, sum(nl))
		pb <- pbCreate(tr$n, label='cover', progress=progress)
		for (i in 1:tr$n) {
			v <- getValues( rasters[[1]], row=tr$row[i], nrows=tr$nrows[i] )
			v2 <- v
			for (j in 2:length(rasters)) {
				v2[] <- getValues(rasters[[j]], row=tr$row[i], nrows=tr$nrows[i])
				v[is.na(v)] <- v2[is.na(v)]
			}	
			outRaster <- writeValues(outRaster, v, tr$row[i])
			pbStep(pb, i) 
		}
		pbClose(pb)
		outRaster <- writeStop(outRaster)
	}
	return(outRaster)
}
)

