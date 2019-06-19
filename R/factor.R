# Author: Robert J. Hijmans
# Date : February 2010 / June 2012
# Version 1.0
# Licence GPL v3



factorValues <- function(x, v, layer=1, att=NULL, append.names=FALSE) {
	stopifnot(is.factor(x)[layer])
	rat <- levels(x)[[layer]]
	if (!is.data.frame(rat)) {
		rat <- rat[[1]]
	}
#	if (colnames(rat)[2]=='WEIGHT') {
#		i <- which(match(rat$ID, round(v))==1)
#	} else {
		i <- match(round(v), rat$ID)
#	}
	r <- rat[i, -1, drop=FALSE]

	rownames(r) <- NULL
	if (!is.null(att)) {
		if (is.character(att)) {
			att <- stats::na.omit(match(att, colnames(r)))
			if (length(att)	== 0) {
				warning("att does not includes valid names")
			} else {
				r <- r[, att, drop=FALSE]
			}
		} else {
			r <- r[, att, drop=FALSE]
		}
	}
	if (append.names) {
		colnames(r) <- paste(names(x)[layer], colnames(r), sep="_")
	}
	r
}



.insertFacts <- function(x, v, lyrs) {
	facts <- is.factor(x)[lyrs]
	if (!any(facts)) {
		return(v)
	}
	i <- which(facts)
	v <- lapply(1:length(facts), 
		function(i) {
			if (facts[i]) {
				data.frame(factorValues(x, v[, i], i, append.names=TRUE))
			} else {
				v[, i, drop=FALSE]
			}
		} )
	do.call(data.frame, v)
}


setMethod('is.factor', signature(x='Raster'), 
	function(x) {
		f <- x@data@isfactor
		nl <- nlayers(x)
		if (length(f) < nl) {
			f <- c(f, rep(FALSE, nl))[1:nl]
		}
		f
	}
)

setMethod('is.factor', signature(x='RasterStack'), 
	function(x) {
		if (nlayers(x) > 0) {
			s <- sapply(x@layers, function(x) x@data@isfactor)
			return(s)
		} else {
			return(FALSE)
		}
	}
)


if (!isGeneric("levels")) {
	setGeneric("levels", function(x)
		standardGeneric("levels"))
}	

setMethod('levels', signature(x='Raster'), 
	function(x) {
		f <- is.factor(x)
		if (any(f)) {
			if (inherits(x, 'RasterStack')) {
				return( sapply(x@layers, function(i) i@data@attributes)  )
			} else {
				return(x@data@attributes)
			}
		} else {
			return(NULL)
		}
	}
)



.checkLevels <- function(old, newv) {
	if (! is.data.frame(newv)) { 
		stop('new raster attributes (factor values) should be in a data.frame (inside a list)')
	}
	if (! ncol(newv) > 0) {
		stop('the number of columns in the raster attributes (factors) data.frame should be > 0')
	}
	if (! colnames(newv)[1] == c('ID')) {
		stop('the first column name of the raster attributes (factors) data.frame should be "ID"')
	}
	
	if (!is.null(old)) {
#		if (colnames(newv)[2] == 'WEIGHT') {
#			if (nrow(newv) < nrow(old)) {
#				warning('the number of rows in the raster attributes (factors) data.frame is lower than expected (values missing?)')
#			}
#			if (! all(unique(sort(newv[,1])) == sort(unique(old[,1])))) {
#				warning('the values in the "ID" column in the raster attributes (factors) data.frame have changed')
#			}
	
#		} else {
		
			if (! nrow(newv) == nrow(old)) {
				warning('the number of rows in the raster attributes (factors) data.frame is unexpected')
			}
			if (! all(sort(newv[,1]) == sort(old[,1]))) {
				warning('the values in the "ID" column in the raster attributes (factors) data.frame have changed')
			}
#		}
	}
	newv[, 1] <- as.integer(newv[, 1])
#	if (colnames(newv)[2] == 'WEIGHT') {
#		newv[, 2] <- as.numeric(newv[, 2])
#	}
	newv
}


setMethod('levels<-', signature(x='Raster'), 
	function(x, value) {
		
		if (is.null(value)) {
			return(x)
		}
		
		isfact <- is.factor(x)

		if (inherits(x, 'RasterLayer')) {
			if (!is.data.frame(value)) {
				if (is.list(value)) {
					value <- value[[1]]
				}
			}
			value <- .checkLevels(levels(x)[[1]], value)
			x@data@attributes <- list(value)
			x@data@isfactor <- TRUE
			return(x)
		} 
		
		i <- ! sapply(value, is.null)
		if ( any(i) ) {
			stopifnot (length(value) == nlayers(x))
			levs <- levels(x)
			for (j in which(i)) {
				value[[j]] <- .checkLevels(levs[[j]], value[[j]])
			}
			x@data@attributes <- value
			x@data@isfactor <- i
		} else {
			x@data@attributes <- list()		
		}
		x@data@isfactor <- i
		return(x)		
	}
)


setMethod('as.factor', signature(x='RasterLayer'), 
	function(x) {
		ratify(x)
	}
)



if (!isGeneric("asFactor")) {
	setGeneric("asFactor", function(x, ...)
		standardGeneric("asFactor"))
}

setMethod('asFactor', signature(x='RasterLayer'), 
	function(x, value=NULL, ...) {
		#warning("please use as.factor")
		x@data@isfactor <- TRUE
		if (is.null(value) ) {
			#x <- round(x) #this makes methods::slot( isfactor FALSE again
			x@data@attributes <- list(data.frame(VALUE=unique(x)))
		} else {
			x@data@attributes <- value
		}	
		return(x)
	}
)
