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

	n <- n * nlayers(x)
	memneed <- ncell(x) * n * 8

	if (.estimateMem()) {

		if ( .Platform$OS.type == "windows" ) {

			mem <- system2("wmic", args = "OS get FreePhysicalMemory /Value", stdout = TRUE)
			mem3 <- gsub("\r", "", mem[3])
			mem3 <- gsub("FreePhysicalMemory=", "", mem3)
			memavail <- as.numeric(mem3) * 1024

			#memavail <- 0.5 * (utils::memory.size(NA) - utils::memory.size(FALSE))
		} else { #if ( .Platform$OS.type == "unix" ) {
			memavail <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE))
		} #else {
			# mac? or is that also "unix"
		#}

		# can't use all of it
		memavail <- 0.75 * memavail

		#print(paste("mem available:", memavail))
		#print(paste("mem needed:", memneed))


		if (memneed > memavail) {
			# options(rasterChunkSize = memavail * 0.5 )
			return(FALSE)
		} else {
			return(TRUE)
		}

	} else {

		if ( memneed > .maxmemory() ) {
			return(FALSE)
		} else {
			return(TRUE)
		}
	}
}

