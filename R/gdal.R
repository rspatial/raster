# Author: Robert J. Hijmans
# Date : September 2012
# Version 1.0
# Licence GPL v3

.requireRgdal <- function(stopIfAbsent=TRUE) {
	TRUE
}

# .requireRgdal <- function(stopIfAbsent=TRUE) {
	
	# y <- getOption('rasterGDALLoaded')

	# suppressWarnings(x <- isTRUE( try( requireNamespace("rgdal", quietly=TRUE ) ) ))
	
	# if (! isTRUE(y) ) {
		
		# if (x) {
			# #pkg.info <- utils::packageDescription('rgdal') 
			# #test <- utils::compareVersion(pkg.info[["Version"]], "0.7-21") > 0
			# #if (!test) {
			# #	stop('you use rgdal version: ', pkg.info[["Version"]], '\nYou need version 0.7-22 or higher')
			# #}
			# options('rasterGDALLoaded'=TRUE)
			# return(TRUE)
			
		# } else if (stopIfAbsent) {
			# stop("package 'rgdal' is not available")
		# } else {
			# return(FALSE)
		# }
	# }

	# return(TRUE)
# }

.useproj6 <- function() {
	TRUE
}

# .useproj6 <- function() {
	# pkg.info <- utils::packageDescription('rgdal') 
	# new_rgdal <- utils::compareVersion(pkg.info[["Version"]], "1.5-7") > 0
	# if (new_rgdal) {
		# if (rgdal::new_proj_and_gdal()) {
			# return (TRUE)
		# } else {
			# return (FALSE)
		# }
	# } else {
		# return (FALSE)
	# }
# }


