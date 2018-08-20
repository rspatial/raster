# Author: Robert J. Hijmans
# Date: March 2013
# Version 1.0
# Licence GPL v3

.rasterObjectFromCDF_GMT <- function(nc) {

	stopifnot(requireNamespace("ncdf4"))

	dims <- ncdf4::ncvar_get(nc, "dimension", 1)
	xr <- ncdf4::ncvar_get(nc, "x_range", 1)
	yr <- ncdf4::ncvar_get(nc, "y_range", 1)
	zr <- ncdf4::ncvar_get(nc, "z_range", 1)
	sp <- ncdf4::ncvar_get(nc, "spacing", 1)
	
	zvar = 'z'
	crs <- NA
	if (xr[1] > -181 & xr[2] < 181 & yr[1] > -91 & yr[2] < 91 ) {
		crs <- "+proj=longlat +datum=WGS84"
	}

	dif1 <- abs(((xr[2] - xr[1]) / dims[1]) - sp[2])
	dif2 <- abs(((xr[2] - xr[1]) / (dims[1]-1)) - sp[2])
	
	if (dif1 < dif2) {  # 30 sec GEBCO data
		r <- raster(xmn=xr[1], xmx=xr[2], ymn=yr[1], ymx=yr[2], ncol=dims[1], nrow=dims[2], crs=crs)
	} else {  # 1 min data 
		resx <- (xr[2] - xr[1]) / (dims[1]-1)
		resy <- (yr[2] - yr[1]) / (dims[2]-1)
		r <- raster(xmn=xr[1]-(0.5*resx), xmx=xr[2]+(0.5*resx), ymn=yr[1]-(0.5*resy), ymx=yr[2]+(0.5*resy), ncol=dims[1], nrow=dims[2], crs=crs)
	}
	
	r@file@name <- nc$filename
	r@file@toptobottom <- FALSE
	attr(r@data, "zvar") <- zvar
	attr(r@data, "dim3") <- 1
	r@file@driver <- "netcdf"
	r@data@fromdisk <- TRUE
	return(r)
}

