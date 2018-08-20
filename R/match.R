# Author: Robert J. Hijmans
# Date : October 2011
# October 2011
# version 1
# Licence GPL v3


if (!isGeneric("%in%")) {
	setGeneric("%in%", function(x, table)
		standardGeneric("%in%"))
}	

setMethod("%in%", signature(x='Raster', table='ANY'),
	function(x, table) {
		calc(x, function(x) x %in% table)
	}
)

if (!isGeneric("match")) {
	setGeneric("match", function(x, table, nomatch=NA_integer_, incomparables=NULL)
		standardGeneric("match"))
}	


setMethod("match", signature(x='Raster', table='ANY', nomatch='ANY', incomparables='ANY'),
	function(x, table, nomatch, incomparables) {
		calc(x, function(x) match(x, table, nomatch, incomparables))
	}
)

