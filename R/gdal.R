# Author: Robert J. Hijmans
# Date : September 2012
# Version 1.0
# Licence GPL v3

.requireRgdal <- function(stopIfAbsent=TRUE) {
	
	y <- getOption('rasterGDALLoaded')

	w <- getOption('warn')
	options('warn'=-1) 
	x <- isTRUE( try( requireNamespace("rgdal", quietly=TRUE ) ) )
	options('warn'= w) 
	
	if (! isTRUE(y) ) {
		
		if (x) {
			#pkg.info <- utils::packageDescription('rgdal') 
			#test <- utils::compareVersion(pkg.info[["Version"]], "0.7-21") > 0
			#if (!test) {
			#	stop('you use rgdal version: ', pkg.info[["Version"]], '\nYou need version 0.7-22 or higher')
			#}
			options('rasterGDALLoaded'=TRUE)
			return(TRUE)
			
		} else if (stopIfAbsent) {
			stop("package 'rgdal' is not available")
		} else {
			return(FALSE)
		}
	}
	
	
	return(TRUE)
}

