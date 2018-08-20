# Author: Robert J. Hijmans
# Date : Febrary 2009
# Version 0.9
# Licence GPL v3


sampleInt <- function(n, size, replace=FALSE) {
	
	n <- round(n[1])
	size <- round(size[1])
	
	stopifnot(n > 0)
	stopifnot(size > 0)
		
	if (!replace) {
		switched <- FALSE
		done <- FALSE
		if (size > (0.66 * n)) { 
			if (size > n ) {
				warning('size changed to n because it cannot be larger than n when replace is FALSE')
				size <- n
			}
			if (size == n) {
				done <- TRUE
			}
			switched <- TRUE
			size <- n - size
		}
		samp <- NULL

		while (! done) {
			f <- ceiling(stats::runif(size * 1.1) * n)
			samp <- unique(c(samp, f))
			if (length(samp) >= size) {
				samp <- samp[1:size]
				done <- TRUE
			}
		}
		if (switched) { 
			if (!is.null(samp)) {
				samp <- (1:n)[-samp]
				lsp <- length(samp)
				samp <- samp[sample.int(lsp)]
			} else {
				samp <- sample.int(n)
			}
		}
		
	} else {
		samp <- ceiling(stats::runif( size ) * n)
	}
	
	return( samp )
}
