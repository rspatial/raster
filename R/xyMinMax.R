# Author: Robert J. Hijmans
# Date :  October 2008
# Version 0.9
# Licence GPL v3



setMethod('xmin', signature(x='BasicRaster'), 
function(x) {
	return(extent(x)@xmin)
})

setMethod('xmax', signature(x='BasicRaster'), 
function(x) {
	return(extent(x)@xmax)
})

setMethod('ymin', signature(x='BasicRaster'), 
function(x) {
	return(extent(x)@ymin)
})

setMethod('ymax', signature(x='BasicRaster'), 
function(x) {
	return(extent(x)@ymax)
})

setMethod('xmin', signature(x='Extent'), 
function(x) {
	return(x@xmin)
})

setMethod('xmax', signature(x='Extent'), 
function(x) {
	return(x@xmax)
})

setMethod('ymin', signature(x='Extent'), 
function(x) {
	return(x@ymin)
})

setMethod('ymax', signature(x='Extent'), 
function(x) {
	return(x@ymax)
})


setMethod('xmin', signature(x='Spatial'), 
function(x) {
	return(extent(x)@xmin)
})

setMethod('xmax', signature(x='Spatial'), 
function(x) {
	return(extent(x)@xmax)
})

setMethod('ymin', signature(x='Spatial'), 
function(x) {
	return(extent(x)@ymin)
})

setMethod('ymax', signature(x='Spatial'), 
function(x) {
	return(extent(x)@ymax)
})


