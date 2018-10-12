# Author: Robert J. Hijmans
# Date: Sept 2009 / revised June 2010
# Version 1.0
# Licence GPL v3


.stackCDF <- function(filename, varname='', bands='') {

	stopifnot(requireNamespace("ncdf4"))

	nc <- ncdf4::nc_open(filename, suppress_dimvals = TRUE)
	on.exit( ncdf4::nc_close(nc) )		

	zvar <- .varName(nc, varname)
	dims <- nc$var[[zvar]]$ndims	
	
	dim3 <- 3
	if (dims== 1) { 
		stop('variable only has a single dimension; I cannot make a RasterLayer from this')
	} else if (dims > 3) { 
		dim3 <- dims
		warning(zvar, ' has ', dims, ' dimensions, I am using the last one')
	} else if (dims == 2) {
		return( stack ( raster(filename, varname=zvar )  )  )
	} 
	
	if (is.null(bands)) { bands <- ''}
	if (bands[1] == '') {
		bands = 1 : nc$var[[zvar]]$dim[[dim3]]$len
	}
	r <- raster(filename, varname=zvar, band=bands[1])
	st <- stack( r )
	st@title <- names(r)

	if (length(bands) > 1) {
		st@z <- list( nc$var[[zvar]]$dim[[dim3]]$vals[bands] )
		names(st@z) <- nc$var[[zvar]]$dim[[dim3]]$units
		if ( nc$var[[zvar]]$dim[[dim3]]$name == 'time' ) {	
			try( st <- .doTime(st, nc, zvar, dim3)  )
		}
		nms <- as.character(st@z[[1]])
		st@layers <- lapply(bands, function(x){
											r@data@band <- x;
											r@data@names <- nms[x];
											return(r)} 
										)
	} 
	return( st )
}
	
 #s = .stackCDF(f, varname='uwnd')
