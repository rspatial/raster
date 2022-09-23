# Author: Robert J. Hijmans
# Date : June 2014
# Version 1.0
# Licence GPL v3


.proj4string <- function(x) {
	if (inherits(x, "Spatial")) {
		suppressWarnings(sp::proj4string(x))
	} else {
		.getSRS(x)
	}
}


.CRS <- function(...) {
	suppressWarnings(sp::CRS(...))
}


if (!isGeneric(".project")) {
	setGeneric(".project", function(x, ...)
		standardGeneric(".project"))
}	


setMethod('.project', signature(x='Raster'), 
	function(x, to=NULL, res=NULL, crs=NULL, method="bilinear", alignOnly=FALSE, over=FALSE, filename="", ...)  {
		projectRaster(x, to=to, res=res, crs=crs, method=method, alignOnly=alignOnly, over=over, filename=filename, ...)
	}
)


setMethod('.project', signature(x='SpatialGrid'), 
	function(x, ...)  {
		y <- brick(x)
		#.requireRgdal()
		dots <- list(...)
		if (!is.null(dots$CRSobj) & is.null(dots$crs)) {
			y <- projectRaster(y, crs=dots$CRSobj, ...)
		} else {
			y <- projectRaster(y, ...)
		}
		as(y, class(x))
	}
)

setMethod('.project', signature(x='SpatialPixels'), 
	function(x, ...)  {
		y <- brick(x)
		#.requireRgdal()
		dots <- list(...)
		if (!is.null(dots$CRSobj) & is.null(dots$crs)) {
			y <- projectRaster(y, crs=dots$CRSobj, ...)
		} else {
			y <- projectRaster(y, ...)
		}
		as(y, class(x))
	}
)


setMethod('.project', signature(x='Spatial'), 
	function(x, crs, ...)  {
		#.requireRgdal()
		if (!is.null(list(...)$CRSobj)) {
			crs <- list(...)$CRSobj
		}
		v <- project(x, projection(crs))
		as(v, "Spatial")
		#sp::spTransform(x, CRSobj=crs(crs), ...)
	}
)


