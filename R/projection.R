# Author: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("wkt", signature(obj="Raster"), 
	function(obj) {
		w <- comment(obj@crs)
		if (is.null(w)) {
			return (obj@crs@projargs)
		} else {
			return(w)
		}
	}
)


.getCRS <- function(x) {
	#srs <- c(x@srs, "")
	#.makeCRS(srs[1], srs[2])
	x@crs
}

.srs_from_sp <- function(x) {
	crs <- x@proj4string
	pj <- crs@projargs
	wk <- wkt(crs)
	return(c(pj, wk))
}


.oldproj4string <- function(x) {
	if (inherits(x, "Spatial")) {
		crs <- x@proj4string
	} else if (inherits(x, "CRS")) {
		crs <- x
	} else if (is.character(x)) {
		crs <- CRS(x)
	} else {
		crs <- x@crs
	}
	crs@projargs
}

.makeCRS <- function(user="", prj="", wkt="") {
	if (wkt != "") {
		CRS(SRS_string=wkt)
	} else if (user !="") {
		if (substr(trim(user), 1 ,1) == "+") {
			CRS(user)
		} else {
			CRS(SRS_string=user)
		}
	} else {
		CRS(prj)
	}
}



setMethod("crs", signature("ANY"), 
	function(x, asText=FALSE, ...) {
		projection(x, asText=asText)
	}
)

setMethod("crs<-", signature("BasicRaster", "ANY"), 
	function(x, ..., value) {
		projection(x) <- value
		x
	}
)

#rgdal::showWKT(projection(x)))

setMethod("crs<-", signature("Spatial", "ANY"), 
	function(x, ..., value) {

		if (!inherits(value, "CRS")) {
			value <- .makeCRS(value)
		}	
	
		w <- getOption("warn")
		on.exit(options("warn" = w))
		options("warn"=-1)

		x@proj4string <- value
		x
	}
)

setMethod("is.na", signature(x="CRS"), 
	function(x) {
		is.na(x@projargs)
	}
)


setMethod("as.character", signature(x="CRS"), 
	function(x, ...) {
		x@projargs
	}
)

"projection<-" <- function(x, value) {

	value <- .get_projection(value)
	
	if (inherits(x, "RasterStack")) {
		if (nlayers(x) > 0) {
			for (i in 1:nlayers(x)) {
				x@layers[[i]]@crs <- value
				#x@layers[[i]]@crs <- CRS(value)
			}
		}
	} 
	if (inherits(x, "Spatial")) {
		x@proj4string <- value
	} else {
		x@crs <- value
		#x@crs <- CRS(value)
	}
	return(x)
}


.get_projection <- function(x, ...) {

	if (is.null(x)) {
		x <- CRS()
	} else if (methods::extends(class(x), "BasicRaster")) { 
		x <- x@crs
	} else if (methods::extends(class(x), "Spatial")) { 
		x <- x@proj4string
	} else if (inherits(x, c("sf", "sfc"))) {
		x <- sf::st_crs(x)
		x <- as(x, "CRS") # passes on WKT comment
	} else if (inherits(x, "SpatRaster")) { 
		crs <- crs(x)
		x <- .makeCRS(x[1], x[2])
	} else if (inherits(x, "SpatVector")) { 
		crs <- crs(x)
		x <- .makeCRS(x[1], x[2])
	} else if (is.character(x)) {
		x <- CRS(x)
	}

	if (is.na(x)) {
		x <- CRS()
	}	
	
	x
}



projection <- function(x, asText=TRUE) {

	if (methods::extends(class(x), "BasicRaster")) { 
		x <- x@crs 
	} else if (methods::extends(class(x), "Spatial")) { 
		x <- x@proj4string
	} else if (inherits(x, c("sf", "sfc"))) {
		crs = sf::st_crs(x)
		if (asText) {
			return(crs$proj4string) # extracts proj4string from WKT
		} else {
			return(as(crs, "CRS")) # passes on WKT comment
		}
	} else if (class(x) == "character") { 
		if (asText) {
			return(x)
		} else {
			return( CRS(x) )
		}
	} else if (class(x) != "CRS") { 
		return(as.logical(NA))
	}
	
	if (asText) {
		if (class(x) == "CRS") { 
			if (is.na(x@projargs)) { 
				return(as.character(NA))
			} else {
				return(trim(x@projargs))
			}
		}
	} else if (class(x) != "CRS") { 
		x <- CRS(x)
	}
	return(x)
}


setMethod("proj4string", signature("Raster"), 
# redundant, for compatibility with sp
	function(obj) {
		projection(obj)
	}
)


setMethod("proj4string<-", signature("Raster"), 
# redundant, for compatibility with sp
	function(obj, value) {
		crs(obj) <- value
		obj
	}
)

