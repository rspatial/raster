# Author: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3


# to be removed when released sp has this for crs
#setMethod("wkt", signature(obj="ANY"), 
#	function(obj) {
#		if (!inherits(obj, "CRS")) {
#			obj <- obj@crs
#		} else if (inherits(obj, c("sf", "sfc"))) {
#			obj <- sf::st_crs(obj)
#			obj <- as(obj, "CRS") # passes on WKT comment
#		}
#		
#		w <- comment(obj)
#		if (is.null(w)) {
#			warning("no wkt comment")
#			return("")
#		} else {
#			return(w)
#		}
#	}

#)

.CRS <- function(...) { .spCRS(...) }


setMethod("wkt", signature(obj="Raster"), 
	function(obj) {
		#w <- comment(obj@crs)
		#if (is.null(w)) {
		#	warning("no wkt comment")
		#	return("")
		#} else {
		#	return(w)
		#}
		if (.hasSlot(obj, "srs")) {
			terra::crs(obj@srs)
		} else {
			NA @.srs_from_sp(obj@crs)
		}
	}
)





.makeCRS <- function(user="", prj="", wkt="") {
	if (missing(user)) user = ""
	if (is.na(user)) user = ""
	if (is.na(prj)) prj = ""
	if (is.na(wkt)) wkt = ""
	
	if (wkt != "") {
		if (prj != "") {
			.spCRS(prj, SRS_string=wkt)
		} else {
			.spCRS(SRS_string=wkt)		
		}
	} else if (user !="") {
		if (substr(trim(user), 1 ,1) == "+") {
			.spCRS(user)
		} else {
			.spCRS(SRS_string=user)
		}
	} else {
		.spCRS(prj)
	}
}


.getCRS <- function(x) {

	if (methods::extends(class(x), "CRS")) { 
		return(x)
	}
	
	if ((length(x) == 0) || is.null(x)) {
		x <- .spCRS()
	} else if (methods::extends(class(x), "BasicRaster")) { 
		#x <- x@crs
		if (!is.na(x@crs)) {
			x <- x@crs
		} else if (.hasSlot(x, "srs")) {
			x <- .makeCRS(x@srs)
		} else {
			x <- .spCRS()
		}
	} else if (methods::extends(class(x), "Spatial")) { 
		x <- x@proj4string
	} else if (inherits(x, c("sf", "sfc"))) {
		x <- sf::st_crs(x)
		x <- as(x, "CRS") # passes on WKT comment
	} else if (inherits(x, "SpatRaster")) { 
		x <- crs(x)
		x <- .makeCRS(x)
	} else if (inherits(x, "SpatVector")) { 
		x <- crs(x, proj=TRUE)
		x <- .makeCRS(x)
	} else if (is.na(x)) {
		x <- .spCRS()
	} else if (is.character(x)) {
		x <- trimws(x)
		if (x == "") {
			x <- .spCRS()
		} else if (substr(x, 1, 4) == "EPSG") {
			x <- .spCRS(terra::crs(x, proj=TRUE))
		} else if (substr(x, 1, 1) == "+") {
			x <- .spCRS(x)
		} else {
			x <- terra::crs(terra::crs(x), proj=TRUE)
			x <- .spCRS(x)
		}
		#if (trimws(x) == "") {
		#	x <- return(CRS())
		#} else {
		#	wkt <- rgdal::showSRID(x)
		#	x <- .spCRS()
		#	x@projargs <- rgdal::showP4(wkt)
		#	attr(x, "comment") <- wkt
		#}
	} else if (is.numeric(x)) {
		x <- paste0("EPSG:", round(x))
		x <- .spCRS(terra::crs(x, proj=TRUE))
	} else {
		x <- .spCRS()
	} # else if "is .spCRS"
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
				value <- .spCRS()
			} else if (is.character(value)) {
				value <- .spCRS(value)
			} else {
				value <- .spCRS(value)
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

	crsvalue <- .getCRS(value)
	srsvalue <- .getSRS(value)
	
	if (inherits(x, "RasterStack")) {
		if (nlayers(x) > 0) {
			for (i in 1:nlayers(x)) {
			#	x@layers[[i]]@crs <- crsvalue
				if (.hasSlot(x@layers[[i]], "srs")) {
					x@layers[[i]]@srs <- srsvalue
				}
			}
		}
	} 
	if (inherits(x, "Spatial")) {
		x@proj4string <- crsvalue
	} else {
		x@crs <- crsvalue
		if (.hasSlot(x, "srs")) {
			x@srs <- srsvalue
		}
	}
	return(x)
}



projection <- function(x, asText=TRUE) {

	if (methods::extends(class(x), "BasicRaster")) { 
		x <- .getCRS(x) 
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
			return( .spCRS(x) )
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
		x <- .spCRS(x)
	}
	return(x)
}



setMethod("proj4string", signature("BasicRaster"), 
	function(obj) {
		if (.hasSlot(obj, "srs")) {
			p4s <- try(suppressWarnings(terra::crs(obj@srs, proj=TRUE)), silent=TRUE)
			if (inherits(obj, "try-error") || (p4s=="")) {
				p4s <- as.character(NA)
			} 
		} else {
			p4s <- obj@crs@projargs
		}
		p4s
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

