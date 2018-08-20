# Author: Robert J. Hijmans
# Date : October 2011
# Version 1.0
# Licence GPL v3


.commonDataType <- function(dtype) {
	dtype <- as.vector(unlist(dtype, use.names = FALSE))
	dtype <- unique(dtype)
	if (length(dtype)==1) {
		datatype <- dtype
	} else {
		dsize <- dataSize(dtype)
		dtype <- .shortDataType(dtype)
		if (any(dtype == 'FLT')) {
			dsize <- max(dsize[dtype=='FLT'])
			datatype <- paste('FLT', dsize, 'S', sep='')
		} else {
			signed <- dataSigned(dtype)
			dsize <- max(dsize)
			if (all(signed)) {
				datatype <- paste('INT', dsize, 'S', sep='')
			} else if (all(!signed)) {
				datatype <- paste('INT', dsize, 'U', sep='')
			} else {
				dsize <- ifelse(dsize == 1, 2, ifelse(dsize == 2, 4, 8))
				datatype <- paste('INT', dsize, 'S', sep='')
			}
		}
	}
	datatype
}

