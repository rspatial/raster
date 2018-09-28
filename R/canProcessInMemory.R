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
	cells <- ncell(x) * n
	
	if (.estimateMem()) {
	
		if ( .Platform$OS.type == "windows" ) {
			memavail <- 0.5 * (utils::memory.size(NA) - utils::memory.size(FALSE))
		} else { #if ( .Platform$OS.type == "unix" ) {
			memavail <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 524288 #1024^2 / 2
		} #else {
			# mac? or is that also "unix"
		#}
			
		memneed <- cells / 131072 # = (cells * 8) / 1024^2
		if (memneed > memavail) {
			return(FALSE)
		} else {
			return(TRUE)
		}
	
	} else {
		
		if ( cells > .maxmemory() ) {
			return(FALSE) 
		} else {
			return(TRUE)
		}
	}
}

