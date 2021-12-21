# Author: Robert J. Hijmans
# Date :  June 2008
# Version 1.0
# Licence GPL v3

.writeHdrRaster <- function(x, type='raster') {

	rastergrd <- .setFileExtensionHeader(filename(x), type)
	thefile <- file(rastergrd, "w")  # open an txt file connection
	cat("[general]", "\n", file = thefile, sep='')
	cat("creator=R package 'raster'", "\n", file = thefile, sep='')
	cat("created=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile, sep='')

	cat("[georeference]", "\n", file = thefile, sep='')
	cat("nrows=",  nrow(x), "\n", file = thefile, sep='')
	cat("ncols=",  ncol(x), "\n", file = thefile, sep='')
	cat("xmin=", as.character(xmin(x)), "\n", file = thefile, sep='')
	cat("ymin=", as.character(ymin(x)), "\n", file = thefile, sep='')
	cat("xmax=", as.character(xmax(x)), "\n", file = thefile, sep='')
	cat("ymax=", as.character(ymax(x)), "\n", file = thefile, sep='')
	cat("projection=", proj4string(x), "\n", file = thefile, sep='')

	cat("[data]", "\n", file = thefile, sep='')
	cat("datatype=",  x@file@datanotation, "\n", file = thefile, sep='')
	cat("byteorder=", x@file@byteorder, "\n", file = thefile, sep='')
	nl <- nlayers(x)
	cat("nbands=",  nl, "\n", file = thefile, sep='')
	cat("bandorder=",  x@file@bandorder, "\n", file = thefile, sep='')


	# currently only for single layer files!
	if (nl == 1) {
		fact <- is.factor(x)[1]
		cat("categorical=", paste(fact, collapse=':'), "\n", file = thefile, sep='')
		if (any(fact)) {
			r <- x@data@attributes[[1]]
			cat("ratnames=", paste(colnames(r), collapse=':'), "\n", file = thefile, sep='')
			cat("rattypes=", paste(sapply(r, class), collapse=':'), "\n", file = thefile, sep='')
			v <- trim(as.character(as.matrix(r)))
			v <- gsub(":", "~^colon^~", v)
			cat("ratvalues=", paste(v, collapse=':'), "\n", file = thefile, sep='')
		} 
		if (length(x@legend@colortable) > 1) {
			cat("colortable=", paste(x@legend@colortable, collapse=':'), "\n", file = thefile, sep='')
		}		
	}
	
#	cat("levels=",  x@data@levels, "\n", file = thefile, sep='')

	cat("minvalue=",  paste(minValue(x, -1, warn=FALSE), collapse=':'), "\n", file = thefile, sep='')
	cat("maxvalue=",  paste(maxValue(x, -1, warn=FALSE), collapse=':'), "\n", file = thefile, sep='')
	cat("nodatavalue=", .nodatavalue(x), "\n", file = thefile, sep='')
#	cat("Sparse=", x@sparse, "\n", file = thefile, sep='')
#	cat("nCellvals=", x@data@ncellvals, "\n", file = thefile, sep='')	

	cat("[legend]", "\n", file = thefile, sep='')
	cat("legendtype=",  x@legend@type, "\n", file = thefile, sep='')
	cat("values=",  paste(x@legend@values, collapse=':'), "\n", file = thefile, sep='')
	cat("color=",  paste(x@legend@color, collapse=':'), "\n", file = thefile, sep='')

	cat("[description]", "\n", file = thefile, sep='')
	ln <- gsub(":", ".", names(x))
	cat("layername=", paste(ln, collapse=':'), "\n", file = thefile, sep='')
	z <- getZ(x)
	if (! is.null(z)) {
		zname <- names(x@z)[1]
		if (is.null(zname)) {
			zname <- 'z-value'
		}
		zclass <- class(z)
		# suggested by Michael Sumner
		if (inherits(z, "POSIXct")) {  
			z <- format(z, "%Y-%m-%d %H:%M:%S", tz="UTC")
		} else {
			z <- as.character(z)
		}		
		
		cat("zvalues=", paste(c(zname, z), collapse=':'), "\n", file = thefile, sep='')
		cat("zclass=", zclass, "\n", file = thefile, sep='')
	}
	
	a <- NULL
	try( a <- unlist(x@history), silent=TRUE )
	if (!is.null(a)) {
		cat("history=", a, "\n", file = thefile, sep='')
	}
	
	a <- NULL
	try( a <- rapply(x@history, function(x) paste(as.character(x), collapse='#,#')), silent=TRUE )
	if (!is.null(a)) {
		a <- gsub('\n', '#NL#', a)
		type <- rapply(x@history, class)
		type_value <- apply(cbind(type, a), 1, function(x) paste(x, collapse=':'))
		name_type_value <- apply(cbind(names(a), type_value), 1, function(x) paste(x, collapse='='))
		name_type_value <- paste(name_type_value, '\n', sep='')
		cat("[metadata]", "\n", file = thefile, sep='')
		cat(name_type_value, file = thefile, sep='')		
	}
	close(thefile)
	return(TRUE)
}


