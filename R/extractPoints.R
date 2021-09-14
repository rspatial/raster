# Author: Robert J. Hijmans
# Date : November 2008
# Version 1.0
# Licence GPL v3



setMethod('extract', signature(x='Raster', y='matrix'), 
function(x, y, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE, ...){ 
	.xyValues(x, y, method=method, buffer=buffer, small=small, cellnumbers=cellnumbers, fun=fun, na.rm=na.rm, layer=layer, nl=nl, df=df, factors=factors, ...)
})



setMethod('extract', signature(x='Raster', y='data.frame'), 
function(x, y, ...){ 
	return( .xyValues(x, as.matrix(y), ...))
})




setMethod('extract', signature(x='Raster', y='SpatialPoints'), 
function(x, y, ..., df=FALSE, sp=FALSE){ 
	#px <-.getCRS(x, asText=FALSE)
	px <-.getCRS(x)
	comp <- compareCRS(px,.getCRS(y), unknown=TRUE)
	if (!comp) {
		if (!.requireRgdal()) {
			warning('CRS of SpatialPoints and rater do not match')
		} else {
			warning('Transforming SpatialPoints to the crs of the Raster')
			y <- sp::spTranform(y, px)
		}
	}
	if (sp) {
		v <- .xyValues(x, sp::coordinates(y)[,1:2,drop=FALSE], ..., df=TRUE)
		if (!.hasSlot(y, 'data')) {
			y <- sp::SpatialPointsDataFrame(y,  v[, -1, drop=FALSE])
		} else {
			y@data <- cbind(y@data, v[, -1, drop=FALSE])
		}
		return(y)
	} else {
		.xyValues(x, sp::coordinates(y)[,1:2,drop=FALSE], ..., df=df)
	}
})


	
.xyValues <- function(object, xy, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE, sp=FALSE, ...) { 


	nlyrs <- nlayers(object)
	if (nlyrs > 1) {
		if (missing(layer)) { layer <- 1 } 
		if (missing(nl)) { nl <- nlyrs } 
		layer <- min(max(1, round(layer)), nlyrs)
		nl <- min(max(1, round(nl)), nlyrs-layer+1)
	} else {
		layer <- 1
		nl <- 1
	}
	
	if (dim(xy)[2] != 2) {
		stop('xy should have 2 columns only.\nFound these dimensions: ', paste(dim(xy), collapse=', ') )
	}
		
	if (! is.null(buffer)) {
		if (method != 'simple') { 
			warning('method argument is ignored when a buffer is used') 
		}
		res <- .xyvBuf(object, xy, buffer, fun, na.rm, layer=layer, nl=nl, cellnumbers=cellnumbers, small=small) 		
		
	} else if (method == 'bilinear') {
		res <- .bilinearValue(object, xy, layer=layer, n=nl) 
		if (cellnumbers) {
			warning("'cellnumbers' does not apply for bilinear values")
		}

	} else if (method=='simple') {
		cells <- cellFromXY(object, xy)
		res <- .cellValues(object, cells, layer=layer, nl=nl) 
		if (cellnumbers) {			
			res <- cbind(cells, res)
			if (ncol(res) == 2) {
				colnames(res)[2] <- names(object)[layer]
			} 
		}
			
	} else {
		stop('invalid "method" argument. Should be simple or bilinear.')
	}
	
	if (df) {
		if (is.list(res)) {
			res <- lapply(1:length(res), function(x) if (length(res[[x]]) > 0) cbind(ID=x, res[[x]]))
			res <- do.call(rbind, res)
			rownames(res) <- NULL
		} else {
			res <- data.frame(cbind(ID=1:NROW(res), res))
		}
		lyrs <- layer:(layer-1+nl)
		if (cellnumbers) {
			cn <- c('ID', 'cells', names(object)[lyrs])
		} else {
			cn <- c('ID', names(object)[lyrs])		
		}
		colnames(res) <- cn

		if (any(is.factor(object)) & factors) {
			v <- res[, -1, drop=FALSE]
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(object, v[,1], layer))
			} else {
				v <- .insertFacts(object, v, lyrs)
			}
			res <- data.frame(res[,1,drop=FALSE], v)
		}
	}
	
	res
}


