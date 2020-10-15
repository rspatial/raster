# raster package
# Author: Robert J. Hijmans
# Date :  September 2009
# Version 0.9
# Licence GPL v3

# this function adds the working directory to a filename, if the filename has no path name 
# and, thus, presumably exists in the working directory.
# Storing the full file name is to avoid that a filename becomes invalid if the working directory 
# changes during an R session

.fullFilename <- function(x, expand=FALSE) {
	x <- trim(x)
	if (identical(basename(x), x)) {
		# exclude PG:xxx and perhaps others
		if (length(grep(":", x)) == 0) {
			x <- file.path(getwd(), x)
		}
	}
	if (expand) {
		x <- path.expand(x)
	}
	return(x)
}
