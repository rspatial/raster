# Author: Robert J. Hijmans
# Date :  October 2010
# Version 1.0
# Licence GPL v3
	

setMethod('values', signature(x='Raster'), 
function(x, ...) {
	getValues(x, ...)
})
	


setMethod('values<-', signature(x='RasterLayer'), 
function(x, value) {
	setValues(x, value)
} )
	
setMethod('values<-', signature(x='RasterBrick'), 
function(x, value) {
	setValues(x, values=value, layer=-1)
} )
	
setMethod('values<-', signature(x='RasterStack'), 
function(x, value) {
	setValues(x, values=value, layer=-1)
} )

setMethod('values<-', signature(x='RasterLayerSparse'), 
function(x, value) {
	setValues(x, value, index=NULL)
} )
