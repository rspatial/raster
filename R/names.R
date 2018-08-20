# Author: Robert J. Hijmans
# Date:  October 2008
# Version 0.9
# Licence GPL v3


.uniqueNames <- function(x, sep='.') {
	y <- as.matrix(table(x))
	y <- y[y[,1] > 1, ,drop=F]
	if (nrow(y) > 0) {
		y <- rownames(y)
		for (i in 1:length(y)) {
			j <- which(x==y[i])
			x[j] <- paste(x[j], sep, 1:length(j), sep='')
		}
	}
	x
}


.goodNames <- function(ln, prefix='layer') {
	validNames(ln, prefix)
}

validNames <- function(x, prefix='layer') {
	x <- trim(as.character(x))
	x[is.na(x)] <- ""
	if (.standardnames()) {
		x[x==''] <- prefix
		x <- make.names(x, unique=FALSE)
	}
	.uniqueNames(x)
}




setMethod('labels', signature(object='Raster'), 
	function(object) { 
		names(object)
	}
)

	
setMethod('names', signature(x='Raster'), 
	function(x) { 
		if (.hasSlot(x@data, 'names')) {
			ln <- x@data@names
		} else {
			ln <- x@layernames		
		}
		ln <- ln[1:nlayers(x)]
		validNames(as.vector(ln))
	}
)


setMethod('names', signature(x='RasterStack'), 
	function(x) { 
		ln <- sapply(x@layers, function(i) i@data@names)
		ln <- ln[1:nlayers(x)]
		validNames(as.vector(ln))
	}
)




setMethod('names<-', signature(x='Raster'), 
	function(x, value)  {
		nl <- nlayers(x)
		if (is.null(value)) {
			value <- rep('', nl)
		} else if (length(value) != nl) {
			stop('incorrect number of layer names')
		}
		value <- validNames(value)
		
		if (inherits(x, 'RasterStack')){
			
			x@layers <- sapply(1:nl, function(i){ 
				r <- x@layers[[i]]
				r@data@names <- value[i]
				r
			})
			
		} else {
			if (.hasSlot(x@data, 'names')) {
				x@data@names <- value
			} else {
				x@layernames <- value		
			}
		}

		return(x)
	}
)

