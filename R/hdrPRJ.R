# Author: Robert J. Hijmans
# Date :  April 2011
# Version 1.0
# Licence GPL v3


.writeHdrPRJ <- function(x, ESRI=TRUE) {
	if (.requireRgdal()) {

		p4s <- try(	rgdal::showWKT(projection(x), file = NULL, morphToESRI = ESRI) )
		if (! inherits(p4s, "try-error")) {
			prjfile <- filename(x)
			extension(prjfile) <- '.prj'
			cat(p4s, file=prjfile)
		} else {
			return(FALSE)
		}
		return(invisible(TRUE))
	} else {
		return(FALSE)
	}
}

	
