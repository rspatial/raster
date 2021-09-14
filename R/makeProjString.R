# Author: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3



.newCRS <- function(projs) {

	if (is.null(projs)) {
		prj <- sp::CRS()
	} else if (is.na(projs)) {
		prj <- sp::CRS()
	} else if (nchar(projs) < 3) { 
		prj <- sp::CRS()
	} else {
		projs <- trim(projs)
		prj <- try(sp::CRS(projs), silent = TRUE)
		if (inherits(prj, "try-error")) { 
			warning(paste(projs, 'is not a valid PROJ.4 crs string')) 
			prj <- sp::CRS()
		}
	}
	return(prj)
}



.makeProj <- function(projection='longlat', ..., ellipsoid="", datum="", asText=TRUE) {
	prj <- rgdal::projInfo("proj")
	ell <- rgdal::projInfo("ellps")
	dat <- rgdal::projInfo("datum")
	projection <- trim(projection)
	ellipsoid <- trim(ellipsoid)
	datum <- trim(datum)
	if (!(projection %in% prj[,1])) {
		stop("unknown projection. See rgdal::projInfo()") 
	} else {
		pstr <- paste('+proj=',projection, sep="")
		projname <- as.vector(prj[which(prj[,1]==projection), 2])
	}
	pargs <- list(...)
	if ( length(pargs) > 0 ) {
		for (i in 1:length(pargs)) {
			pstr <- paste(pstr, ' +', pargs[[i]], sep="")
		}
	}
	if (ellipsoid != "") {
		if (!(ellipsoid %in% ell[,1])) { 
			stop("unknown ellipsoid. See rgdal::projInfo('ellps')") 
		} else {
			pstr <- paste(pstr, " +ellps=", ellipsoid, sep="")
#			ellipname <- ell[which(ell[,1]==ellipsoid), 2]
		}
	}
	if (datum != "") {
		if (!(datum %in% dat[,1])) { 
			stop("unknown datum. See rgdal::projInfo('datum')") 
		} else {
			pstr <- paste(pstr, " +datum=", datum, sep="")
#			datumname <- as.vector(dat[which(dat[,1]==datum), 2])
		}
	}
	# cat("Projection: ", projname[1], "\n")
	crs <- .newCRS(pstr)
	if (asText) { 
		return(trim(crs@projargs))
	} else {
		return(crs)
	}
}

