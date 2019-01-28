# Authors: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3


#.RAMavailable <- function(defmem=.maxmemory()) {
#
#	if (useC) {
#		.availableRAM(defmem)
#	} else {
		# essentially the same results as above, but slower
		
#		if ( .Platform$OS.type == "windows" ) {
#			mem <- system2("wmic", args = "OS get FreePhysicalMemory /Value", stdout = TRUE)
#			mem3 <- gsub("\r", "", mem[3])
#			mem3 <- gsub("FreePhysicalMemory=", "", mem3)
#			memavail <- as.numeric(mem3) * 1024
			#memavail <- 0.5 * (utils::memory.size(NA) - utils::memory.size(FALSE))
#		} else if ( .Platform$OS.type == "unix" ) {
			# mac is also "unix" and this does not work on mac
#			memavail <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE))
#		} else {
			#don't know how to do this on a mac
#			memavail <- defmem
#		}
#	}
#	memavail
#}


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

	nc <- ncell(x)
	# avoid vectors that are too long
	
	n <- n * nlayers(x)
	memneed <- nc * n * 8
	maxmem <- .maxmemory()
	memavail <- .availableRAM(maxmem)
	if (verbose) {
		gb <- 1073741824
		cat("memory stats in GB")
		cat(paste("\nmem available:", round(memavail / gb, 2)))
		cat(paste0("\n        ", round(100*.memfrac()) , "%  : ", round(.memfrac() * memavail / gb, 2)))
		cat(paste("\nmem needed   :", round(memneed / gb, 2)))
		cat(paste("\nmax allowed  :", round(maxmem / gb, 2), " (if available)\n"))
	}
	if (nc > (2^31 -1)) return(FALSE)

	# can't use all of it; default is 60%
	memavail <- .memfrac() * memavail

	# the below allows you to safely set a high maxmem
	# but still limit total mem use
	memavail <- min(memavail, maxmem)

	if (memneed > memavail) {
		# new (hidden) option; the 0.25 could be another option
		# now you can only make it lower via chunksize
		options(rasterChunk = min(.chunksize(), memavail * 0.25))
		return(FALSE)
	} else {
		return(TRUE)
	}

}

