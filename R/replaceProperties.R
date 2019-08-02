# Author: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("ncol<-", signature('BasicRaster', 'numeric'), 
	function(x, ..., value) {
		dim(x) <- c(nrow(x), value)
		return(x)
	}
)

setMethod("nrow<-", signature('BasicRaster', 'numeric'), 
	function(x, ..., value) {
		dim(x) <- c(value, ncol(x))
		return(x)
	}
)


setMethod("xmin<-", signature('Extent', 'numeric'), 
	function(x, ..., value) {
		x@xmin <- value
		return(x)
	}
)
setMethod("xmax<-", signature('Extent', 'numeric'), 
	function(x, ..., value) {
		x@xmax <- value
		return(x)
	}
)
setMethod("ymin<-", signature('Extent', 'numeric'), 
	function(x, ..., value) {
		x@ymin <- value
		return(x)
	}
)
setMethod("ymax<-", signature('Extent', 'numeric'), 
	function(x, ..., value) {
		x@ymax <- value
		return(x)
	}
)

setMethod("xmin<-", signature('BasicRaster', 'numeric'), 
	function(x, ..., value) {
		x@extent@xmin <- value
		return(x)
	}
)

setMethod("xmax<-", signature('BasicRaster', 'numeric'), 
	function(x, ..., value) {
		x@extent@xmax <- value
		return(x)
	}
)


setMethod("ymin<-", signature('BasicRaster', 'numeric'), 
	function(x, ..., value) {
		x@extent@ymin <- value
		return(x)
	}
)


setMethod("ymax<-", signature('BasicRaster', 'numeric'), 
	function(x, ..., value) {
		x@extent@ymax <- value
		return(x)
	}
)

