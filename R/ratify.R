# Author: Robert J. Hijmans
# Date : June 2012
# Version 1.0
# Licence GPL v3

if (!isGeneric("ratify")) {setGeneric("ratify", function(x, ...)		standardGeneric("ratify"))}	

setMethod("ratify", signature(x="Raster"), 
	function(x, filename="", count=FALSE, ...) {
		stopifnot(nlayers(x) == 1)
		if (count) {
			f <- freq(x, useNA='no')
			f <- data.frame(f)
			colnames(f) <- c('ID', 'COUNT')
		} else {
			f <- data.frame(ID=unique(x))
		}
		x@data@isfactor <- TRUE
		x@data@attributes <- list(f)
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
			# only native format stores this, hence re-assign these:
			x@data@isfactor <- TRUE
			x@data@attributes <- list(f)	
		}
		return(x)
	}
)

.unweightRAT <- function(rat, fun='mean') {

	fun <- .makeTextFun(fun)
	x <- stats::na.omit(rat) 
	cols <- 3:ncol(x)

	cls <- sapply(x[,cols,drop=FALSE], class)
	
	if (fun %in% c('min', 'max')) {
		if (any(cls %in% 'factor')) {
			warning('you cannot use a mean value for a factor')
			i <- which(cls %in% 'factor') + 2
			x[, i] <- NA			
		}
		x <- aggregate(x[,cols], x[,1,drop=FALSE], fun)
		x <- data.frame(ID=x[,1], COUNT=NA, x[,cols-1])
	} else if (fun == 'mean') {
		if (any(! cls %in% c('integer', 'numeric'))) {
			warning('you cannot use a mean value for a variable that is not a number')
			i <- which(! cls %in% c('integer', 'numeric')) + 2
			x[, i] <- NA
		}
		v <- aggregate(x[,2] * x[,cols], x[,1,drop=FALSE], sum)
		w <- aggregate(x[,2], x[,1,drop=FALSE], sum)
		v[,cols-1] <- v[,cols-1]/w[,2]
		x <- cbind(ID=v[,1], COUNT=NA, value=v[,cols-1])
	} else if (fun == 'largest') {
		ids <- unique(x[,1])
		j <- list()
		for (i in 1:length(ids)) {
			v <- subset(x, x[,1]==ids[i])
			j[[i]] <- v[which.max(v[,2]), ]
		}
		return( do.call(rbind, j) )
	} else if (fun == 'smallest') {
		ids <- unique(x[,1])
		j <- list()
		for (i in 1:length(ids)) {
			v <- subset(x, x[,1]==ids[i])
			j[[i]] <- v[which.min(v[,2]), ]
		}
		return( do.call(rbind, j) )
	
	} else {
		stop('argument "fun" is not valid (should be "mean", "min", "max", "smallest", or "largest"')
	}
	colnames(x)[cols] <- colnames(rat)[cols]
	merge(unique(rat[,1,drop=FALSE]), x, by=1, all.x=TRUE)
}



deratify <- function(x, att=NULL, layer=1, complete=FALSE, drop=TRUE, fun='mean', filename='', ...) {

	x <- x[[layer]]
	rats <- is.factor(x)

	if (!rats) {	
		warning('This layer is not a factor')
		return(x)
	}
	
	RAT <- levels(x)[[1]]

	if (NCOL(RAT) > 2) {
		if (colnames(RAT)[2] == '_WEIGHT_') {
			levels(x) <- .unweightRAT(RAT, fun)
		}
	} else if (NCOL(RAT) == 1) {
		if (complete) {
			x@data@isfactor <- FALSE
			x@data@attributes <- list()
			return(x)
		} else {
			warning('this layer already has a single factor level (use "complete=TRUE" to remove it)')
			return(x)
		}
	}
	
	
	nms <- colnames(RAT)
	if (!is.null(att)) {
		if (is.character(att)) {
			att <- stats::na.omit(match(att, nms))
			if (length(att) == 0) {
				stop("argument 'att' does not include valid names")
			}
		}
		RAT <- RAT[ , c(1, att), drop=FALSE]
	} 
	
	cc <- 2:ncol(RAT)

	if (drop) {
		for (i in cc) {
			options('warn'=-1) 
			suppressWarnings(v <- as.numeric(as.character(RAT[,i])))
			if (isTRUE(all(RAT[,i] == v))) {
				RAT[,i] <- v
			}
		}
	}
	subs(x, RAT, by=1, which=cc, subsWithNA=TRUE, filename=filename, ...)	
}


