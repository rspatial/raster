# Authors: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3


.RAMavailable <- function(defmem, useC=TRUE) {
	if (useC) {
		memavail <- .availableRAM(defmem)
	} else {
		# essentially the same results as above, but slower
		
		if ( .Platform$OS.type == "windows" ) {
			mem <- system2("wmic", args = "OS get FreePhysicalMemory /Value", stdout = TRUE)
			mem3 <- gsub("\r", "", mem[3])
			mem3 <- gsub("FreePhysicalMemory=", "", mem3)
			memavail <- as.numeric(mem3) * 1024
			#memavail <- 0.5 * (utils::memory.size(NA) - utils::memory.size(FALSE))
		} else if ( .Platform$OS.type == "unix" ) {
			memavail <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE))
		} else {
			#don't know how to do it for mac
			memavail <- defmem
		}
	}
	memavail

}


canProcessInMemory <- function(x, n=4, verbose=FALSE) {

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

	maxmem <- .maxmemory()
	memavail <- .RAMavailable(maxmem, TRUE)
	if (verbose) {
		print(paste("mem available:", memavail))
		print(paste("mem needed:", memneed))
	}
	# the below should not be needed, but
	# this allows you to safely set a high maxmem
	# but still limit total mem use
	memavail <- min(memavail, maxmem)

	# can't use all of it
	memavail <- 0.75 * memavail

	if (memneed > memavail) {
		# options(rasterChunkSize = memavail * 0.25 )
		return(FALSE)
	} else {
		return(TRUE)
	}

}

