# Author: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3


# to be removed when released sp has this for crs
setMethod("wkt", signature(obj="ANY"), 
	function(obj) {
		if (inherits(obj, c("sf", "sfc"))) {
			obj <- sf::st_crs(obj)
			obj <- as(obj, "CRS") # passes on WKT comment
		} else if (!inherits(obj, "CRS")) {
			obj <- .getSRS(obj)
		}
		w <- comment(obj)
		if (is.null(w)) {
			warning("no wkt comment")
			return("")
		} else {
			return(w)
		}
	}
)


setMethod("wkt", signature(obj="Raster"), 
	function(obj) {
		#w <- comment(obj@crs)
		#if (is.null(w)) {
		#	warning("no wkt comment")
		#	return("")
		#} else {
		#	return(w)
		#}
		crs(terra::rast(crs=crs(obj, asText=TRUE)))
	}
)



.srs_from_sp <- function(x) {
	crs <- x@proj4string
	pj <- crs@projargs
	wk <- wkt(crs)
	return(c(pj, wk))
}




.makeCRS <- function(x="") {
	wkt <- crs(rast(crs=x))
	sp::CRS(SRS_string=wkt)
}

.getSRS <- function(x) {
	if (methods::extends(class(x), "CRS")) { 
		a <- attr(x, "comment")
		if (is.null(a)) {
			x@projargs
		} else {
			a
		}

	} else if (is.null(x)) {
		""
	} else if (methods::extends(class(x), "BasicRaster")) { 
		if (.hasSlot(x, "srs")) {
			if (x@srs != "") {
				x@srs
			} else {
				a <- attr(x@crs, "comment")
				if (is.null(a)) {
					x@crs@projargs
				} else {
					a
				}
			}
		} else {
			a <- attr(x@crs, "comment")
			if (is.null(a)) {
				x@crs@projargs
			} else {
				a
			}
		}
	} else if (methods::extends(class(x), "Spatial")) { 
		x <- x@proj4string
		a <- attr(x, "comment")
		if (is.null(a)) {
			x@projargs
		} else {
			a
		}
	} else if (inherits(x, c("sf", "sfc"))) {
		sf::st_crs(x)
	} else if (inherits(x, "SpatRaster")) { 
		crs(x, proj=TRUE)
	} else if (inherits(x, "SpatVector")) { 
		crs(x, proj=TRUE)
	} else if (is.na(x)) {
		""
	} else if (is.character(x)) {
		trimws(x)
		#r <- ""
		#try(r <- crs(rast(crs=trimws(x)), proj=TRUE))
		#r
		
#		if (x == "") {
#			x <- .CRS()
#		} else if (substr(x, 1, 1) == "+") {
#			x <- .CRS(x)
#		} else {
#			x <- .CRS(SRS_string = x)
#		}
		#if (trimws(x) == "") {
		#	x <- return(CRS())
		#} else {
		#	wkt <- rgdal::showSRID(x)
		#	x <- .CRS()
		#	x@projargs <- rgdal::showP4(wkt)
		#	attr(x, "comment") <- wkt
		#}
	} else if (is.numeric(x)) {
		.getSRS(paste0("EPSG:", round(x)))
	} else {
		""
	} # else if "is .CRS"
}	


.getCRS <- function(x) {

	if (methods::extends(class(x), "CRS")) { 
		return(x)
	}

	if (is.null(x)) {
		x <- .CRS()
	} else if (methods::extends(class(x), "BasicRaster")) { 
		if (!is.na(x@crs)) {
			return(x@crs)
		} 
		if (.hasSlot(x, "srs")) {
			x <- x@srs
		} else {
			x <- as.character(NA)
		}
	} else if (methods::extends(class(x), "Spatial")) { 
		x <- x@proj4string
	} else if (inherits(x, c("sf", "sfc"))) {
		x <- sf::st_crs(x)
		x <- as(x, "CRS") # passes on WKT comment
	} else if (inherits(x, "SpatRaster")) { 
		x <- .makeCRS(crs(x))
	} else if (inherits(x, "SpatVector")) { 
		x <- .makeCRS(crs(x))
	} else if (is.na(x)) {
		x <- .CRS()
	} else if (is.character(x)) {
		x <- trimws(x)
		if (x == "") {
			x <- .CRS()
		} else if (substr(x, 1, 1) == "+") {
			x <- .CRS(x)
		} else {
			x <- .CRS(SRS_string = x)
		}
		#if (trimws(x) == "") {
		#	x <- return(CRS())
		#} else {
		#	wkt <- rgdal::showSRID(x)
		#	x <- .CRS()
		#	x@projargs <- rgdal::showP4(wkt)
		#	attr(x, "comment") <- wkt
		#}
	} else if (is.numeric(x)) {
		x <- paste0("EPSG:", round(x))
		x <- .CRS(SRS_string = x)	
	}
	
	if (!inherits(x, "CRS")) { 
		x <- .CRS(x)
	}
	x
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
			if (is.na(value)) {
				value <- .CRS()
			} else if (is.character(value)) {
				value <- .CRS(value)
			} else {
				value <- .CRS(value)
			}
		}
	
		suppressWarnings(x@proj4string <- value)
		x
	}
)

setMethod("is.na", signature(x="CRS"), 
	function(x) {
		is.na(x@projargs)
	}
)


"projection<-" <- function(x, value) {

#	value <- .getCRS(value)
	if (inherits(value, "CRS")) {
		cvalue <- value
		svalue <- .getSRS(value)
	} else {
		svalue <- .getSRS(value)
		cvalue <- .makeCRS(svalue)
	}
	
	if (inherits(x, "RasterStack")) {
		if (nlayers(x) > 0) {
			for (i in 1:nlayers(x)) {
				x@layers[[i]]@crs <- cvalue
				if (.hasSlot(x, "srs")) {
					x@layers[[i]]@srs <- svalue
				}
			}
		}
	} 
	if (inherits(x, "Spatial")) {
		x@proj4string <- cvalue
	} else {
		x@crs <- cvalue
		if (.hasSlot(x, "srs")) {
			x@srs <- svalue
		}
	}
	return(x)
}



projection <- function(x, asText=TRUE) {

	if (methods::extends(class(x), "BasicRaster")) { 
		hassrs <- .hasSlot(x, "srs")
		hascrs <- .hasSlot(x, "crs")
		
		if (hassrs & hascrs) {
			if (x@srs != "") {
				x <- x@srs
			} else {
				if (asText) {
					a <- attr(x@crs, "comment")
					if (is.null(a)) {
						x <- x@crs@projargs
					} else {
						x <- a
					}
				} else {
					return(x@crs)
				}
			}
		} else if (hassrs) {
			x <- x@srs 
		} else {
			if (asText) {
				a <- attr(x@crs, "comment")
				if (is.null(a)) {
					x <- x@crs@projargs
				} else {
					x <- a
				}
			} else {
				return(x@crs)
			}
		}
	} else if (methods::extends(class(x), "Spatial")) { 
		x <- x@proj4string
	} else if (inherits(x, c("sf", "sfc"))) {
		crs = sf::st_crs(x)
		if (asText) {
			return(crs$proj4string) # extracts  sp::proj4string from WKT
		} else {
			return(as(crs, "CRS")) # passes on WKT comment
		}
	} else if (inherits(x, "character")) { 
		if (asText) {
			return(x)
		} else {
			return( .CRS(x) )
		}
	} else if (!inherits(x, "CRS")) { 
		return(as.logical(NA))
	}
	
	if (asText) {
		if (inherits(x, "CRS")) { 
			if (is.na(x@projargs)) { 
				return(as.character(NA))
			} else {
				return(trim(x@projargs))
			}
		}
	} else if (!inherits(x, "CRS")) { 
		x <- .CRS(x)
	}
	return(x)
}



setMethod("proj4string", signature("BasicRaster"), 
	function(obj) {
		if (!is.na(obj@crs)) {
			x <- obj@crs@projargs
		} else if (.hasSlot(obj, "srs")) {
			x <- obj@srs
			if (x == "") x <- as.character(NA)
		} 
		x
	}
)	

setMethod("as.character", signature("CRS"), 
	function(x, ...) {
		x@projargs
	}
)

setMethod("proj4string", signature("CRS"), 
	function(obj) {
		obj@projargs
	}
)	


setMethod("proj4string<-", signature("Raster"), 
	function(obj, value) {
		crs(obj) <- value
		obj
	}
)

