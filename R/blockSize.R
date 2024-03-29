# Author: Robert J. Hijmans
# Date : November 2009
# Version 0.9
# Licence GPL v3


setMethod("blockSize", signature(x="Raster"),
	function(x, chunksize, n=nlayers(x), minblocks=4, minrows=1) {

		n <- max(n, 1)
		if (missing(chunksize)) {
			bs <- .chunk()
		} else {
			bs <- chunksize
		}

		blockrows <- try(methods::slot(x@file, 'blockrows'), silent=TRUE)
		if (inherits(blockrows, 'try-error')) {
			blockrows <- 1
		}
		blockrows <- max(blockrows, 1)


		nr <- nrow(x)
		size <- min(nr, max(1, floor(bs / (ncol(x) * n * 8))))
		# min number of chunks
		if (size > 1) {
			minblocks <- min(nr, max(1, minblocks))
			size <- min(ceiling(nr/minblocks), size)
		}
		size <- min(max(size, minrows), nr)
		size <- max(minrows, blockrows * round(size / blockrows))

		nb <- ceiling(nr / size)
		row <- (0:(nb-1))*size + 1
		nrows <- rep(size, length(row))

		dif = nb * size - nr
		nrows[length(nrows)] = nrows[length(nrows)] - dif

		return(list(row=row, nrows=nrows, n=nb))
	}
)

