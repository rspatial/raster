# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3

.getVars <- function(v, cn, nc) {
	vl <- length(v)
	v <- unique(v)
	if (is.numeric(v)) {
		v <- round(v)
		v <- v[v>0 & v <= nc]
		if (length(v) < 1) {
			stop('invalid column numbers')
		}
	} else if (is.character(v)) {
		v <- v[v %in% cn]
		if (length(v) < 1) {
			stop('invalid column names')
		}
	}
	v
}


.doSums <- function(sums, cn, dc, x) {
	out <- list()
	for (i in 1:length(sums)) {
		if (length(sums[[i]]) != 2) {
			stop('argument "s" most of be list in which each element is a list of two (fun + varnames)')
		}
		fun = sums[[i]][[1]]
		if (!is.function(fun)) {
			if (is.character(fun)) {
				if (tolower(fun[1]) == 'first') {
					fun <- function(x) x[1]
				} else if  (tolower(fun[1]) == 'last') {
					fun <- function(x) x[length(x)]
				} 
			}
		}
		v <- .getVars(sums[[i]][[2]], cn, ncol(x@data))
		ag <- aggregate(x@data[,v,drop=FALSE], by=list(dc$v), FUN=fun) 
		out[[i]] <- ag[,-1,drop=FALSE]
	}
	do.call(cbind, out)
}


setMethod('aggregate', signature(x='SpatialPolygons'), 
function(x, by=NULL, sums=NULL, dissolve=TRUE, vars=NULL, ...) {
	
	if (!is.null(vars)) {
		if (is.null(by)) {
			by <- vars
		} else {
			stop('do not provide "by" and "vars" arguments')
		}
		warning('Use argument "by" instead of deprecated argument "vars"')
	}
	
	if (!is.null(by)) {
		if (!is.character(by)) {
			# sp::aggregate is not exported 
			# solution by Matt Strimas-Mackey
			spAgg <- get('aggregate', envir=as.environment("package:sp"))
			return( spAgg(x, by, ..., dissolve=dissolve) )
		}
	}
	
	if (dissolve) {
		if (!requireNamespace("rgeos")) {
			warning('Cannot dissolve because the rgeos package is not available')
			dissolve <- FALSE
		}
	}
	
	if (!.hasSlot(x, 'data') ) {
		hd <- FALSE
		if (!is.null(by)) {
			if (length(by) == length(x@polygons)) {
				x <- SpatialPolygonsDataFrame(x, data=data.frame(ID=by))
				by <- 1
			} else if (is.character(by)) {
				stop('character argument for by not understood. It is not length(x) and x has no attributes')
			}
		}
	} else {
		hd <- TRUE
	}
	
	if (isTRUE(is.null(by))) {
		if (dissolve) {
			if (rgeos::version_GEOS() < "3.3.0") {
				x <- rgeos::gUnionCascaded(x)
			} else {
				x <- rgeos::gUnaryUnion(x)
			}
		} else {
			p <- list()
			for (i in 1:length(x)) {
				nsubobs <- length(x@polygons[[i]]@Polygons)
				p <- c(p, lapply(1:nsubobs, function(j) x@polygons[[i]]@Polygons[[j]]))
			}
			x <- SpatialPolygons(list(Polygons(p, '1')), proj4string=proj4string(x))
		}
		#if (hd) {
		#	x <- SpatialPolygonsDataFrame(x, data=data.frame(ID=1))
		#}
		return(x)
		
	} else {
		
		dat <- x@data
		cn <- colnames(dat)
		v <- .getVars(by, cn)
		
		dat <- dat[,v, drop=FALSE]
		crs <- proj4string(x)
		dc <- apply(dat, 1, function(y) paste(as.character(y), collapse='_'))
		dc <- data.frame(oid=1:length(dc), v=as.integer(as.factor(dc)))
		id <- dc[!duplicated(dc$v), , drop=FALSE]

		if (nrow(id) == nrow(dat)) {
			# nothing to aggregate
			if (hd) {
				x@data <- dat
			} else {
				x <- as(x, 'SpatialPolygons')
			}
			return(x)
		}

		id <- id[order(id$v), ]
		dat <- dat[id[,1], ,drop=FALSE]
		
		if (!is.null(sums)) {
			out <- .doSums(sums, cn, dc, x)
			dat <- cbind(dat, out)
		}

		
		if (hd) {
			x <- as(x, 'SpatialPolygons')
		}
		
		if (dissolve) {
			if (rgeos::version_GEOS0() < "3.3.0") {
				x <- lapply(1:nrow(id), function(y) spChFIDs(rgeos::gUnionCascaded(x[dc[dc$v==y,1],]), as.character(y)))
			} else {
			
				x <- lapply(1:nrow(id), 
						function(y) {
							z <- x[dc[dc$v==y, 1], ]
							z <- try( rgeos::gUnaryUnion(z) )
							if (! inherits(z, "try-error")) {
								spChFIDs(z, as.character(y))
							}
						}
					)
			}	
		} else {
			x <- lapply(1:nrow(id), function(y) spChFIDs(aggregate(x[dc[dc$v==y,1],], dissolve=FALSE), as.character(y)))
		}
		
		x <- do.call(rbind, x)
		crs(x) <- crs
		rownames(dat) <- NULL
		SpatialPolygonsDataFrame(x, dat, FALSE)
	}
}
)


setMethod('aggregate', signature(x='SpatialLines'), 
function(x, by=NULL, sums=NULL, ...) {

	if (!is.null(by)) {
		if (!is.character(by)) {
			# sp::aggregate is not exported 
			# solution by Matt Strimas-Mackey
			spAgg <- get('aggregate', envir=as.environment("package:sp"))
			return( spAgg(x, by, ...) )			
		}
	}
	
	if (!.hasSlot(x, 'data') ) {
		hd <- FALSE
		if (!is.null(by)) {
			if (length(by) == length(x@lines)) {
				x <- SpatialLinesDataFrame(x, data=data.frame(ID=by))
				by <- 1
			} else if (is.character(by)) {
				stop('character argument for by not understood. It is not length(x) and x has no attributes')
			}
		}		
	} else {
		hd <- TRUE
	}
	
	if (isTRUE(is.null(by))) {
		p <- list()
		for (i in 1:length(x)) {
			nsubobs <- length(x@lines[[i]]@Lines)
			p <- c(p, lapply(1:nsubobs, function(j) x@lines[[i]]@Lines[[j]]))
		}
		x <- SpatialLines(list(Lines(p, '1')), proj4string=proj4string(x))
		return(x)
		
	} else {
		
		dat <- x@data
		cn <- colnames(dat)
		v <- .getVars(by, cn)
		
		dat <- dat[,v, drop=FALSE]
		crs <- proj4string(x)
		dc <- apply(dat, 1, function(y) paste(as.character(y), collapse='_'))
		dc <- data.frame(oid=1:length(dc), v=as.integer(as.factor(dc)))
		id <- dc[!duplicated(dc$v), , drop=FALSE]

		if (nrow(id) == nrow(dat)) {
			# nothing to aggregate
			if (hd) {
				x@data <- dat
			} else {
				x <- as(x, 'SpatialLines')
			}
			return(x)
		}

		id <- id[order(id$v), ]
		dat <- dat[id[,1], ,drop=FALSE]
		
		if (!is.null(sums)) {
			out <- .doSums(sums, cn, dc, x)
			dat <- cbind(dat, out)
		}
		
		if (hd) {
			x <- as(x, 'SpatialLines')
		}
		x <- lapply(1:nrow(id), function(y) spChFIDs(aggregate(x[dc[dc$v==y,1],]), as.character(y)))
		
		x <- do.call(rbind, x)
		crs(x) <- crs
		rownames(dat) <- NULL
		SpatialLinesDataFrame(x, dat, FALSE)
	}
}
)


