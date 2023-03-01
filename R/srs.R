
.srs_from_sp <- function(x) {
	crs <- x@projargs
	wk <- attr(x, "comment")
	if (!is.null(wk) && (!is.na(wk)) && (wk != "")) {
		wk
	} else {
		crs
	}
}


.getSRS <- function(x) {
	if (methods::extends(class(x), "CRS")) { 
		a <- attr(x, "comment")
		if (is.null(a)) {
			x@projargs
		} else {
			a
		}

	} else if (is.null(x) || (length(x)==0)) {
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
#			x <- .spCRS()
#		} else if (substr(x, 1, 1) == "+") {
#			x <- .spCRS(x)
#		} else {
#			x <- .spCRS(SRS_string = x)
#		}
		#if (trimws(x) == "") {
		#	x <- return(CRS())
		#} else {
		#	wkt <- rgdal::showSRID(x)
		#	x <- .spCRS()
		#	x@projargs <- rgdal::showP4(wkt)
		#	attr(x, "comment") <- wkt
		#}
	} else if (is.numeric(x)) {
		.getSRS(paste0("EPSG:", round(x)))
	} else {
		""
	} # else if "is .spCRS"
}	

