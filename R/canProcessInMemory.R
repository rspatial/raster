# Authors: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3


canProcessInMemory <- function(x, n=4) {


# for testing purposes	
#	rasterOptions(format='GTiff') 
#	requireNamespace("ncdf4")
#	requireNamespace("rgdal")
#	rasterOptions(format='big.matrix')
#	rasterOptions(format='CDF')
#	rasterOptions(overwrite=TRUE)
#  rasterOptions(todisk=TRUE)
#  return(FALSE)
	
	if (.toDisk()) { 
		return(FALSE) 
	} 
	
	n <- n + nlayers(x) - 1
	cells <- round( 1.1 * ncell(x) ) * n

	if ( cells > .maxmemory() ) {
		return(FALSE) 
	} else {
		return(TRUE)
	}
}

#	if (cells > .maxmemory()) {
#		return(FALSE) 
#	} else if ( cells < 1000000 ) {
#		return(TRUE)
#	} else {
#		return(TRUE)
#	}
	
	
#	if (substr( R.Version()$platform, 1, 7) == "i386-pc" ) {
#	# windows, function memory.size  available
#	memneed <- cells * 8 * n / (1024 * 1024)
#	memavail <- 0.5 * (memory.size(NA)-memory.size(FALSE))
#	if (memneed > memavail) {
#		return(FALSE)
#	} else {
#		return(TRUE)
#	}
#   } else {

#	g <- gc()


#  if (.Platform$OS.type == "unix"){
## Memory in KB, from: http://stackoverflow.com/questions/2441046/how-to-get-physical-memory-in-bash
#	mem <- as.numeric(system("grep MemTotal /proc/meminfo | awk '{print $2}'",intern=TRUE))

#	w <- getOption('warn')
#	on.exit(options('warn'= w))
#	options('warn'=-1) 
#	r <- try( matrix(0.1, ncol=n, nrow=cells), silent=TRUE )

#	if (class(r) == "try-error") {
#		return( FALSE )
#		g <- gc()
#		return( TRUE ) 
#	}
