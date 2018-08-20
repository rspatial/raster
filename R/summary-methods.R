# Authors: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3

.addArgs <- function(...) {
	lst <- list(...)
	if (length(lst) > 0 ) {
		i <- sapply(lst, function(x) class(x) %in% c('logical', 'integer', 'numeric'))
		add <- unlist(lst[i], use.names = FALSE)
	} else {
		add <- NULL
	}
	return(add)
}



setMethod("Summary", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){

		fun <- as.character(sys.call()[[1L]])
		
		dots <- list(...)
		if (length(dots) > 0) {
			d <- sapply(dots, function(i) inherits(i, 'Raster'))
			if (any(d)) {
				x <- .makeRasterList(x, dots[d])
				if (length(x) > 1) {
					x <- stack(x)
				} else {
					x <- x[[1]]
				}
			}
			add <- .addArgs(unlist(dots[!d]))
		} else {
			add <- NULL
		}

		if (nlayers(x)==1 & length(add)==0) {
			warning('Nothing to summarize if you provide a single RasterLayer; see cellStats')
			return(x)
		}	
		
		if (fun[1] == 'sum') {
			return(.sum( x, add, na.rm=na.rm))
		} else if (fun[1] == 'min') {
			return(.min( x, add, na.rm=na.rm ))
		} else if (fun[1] == 'max') {
			return(.max( x, add, na.rm=na.rm))
		} else if (fun[1] == 'range') {
			return(.range( x, add, na.rm=na.rm))
		}


		out <- raster(x)
	
		if (canProcessInMemory(x)) {
			
			if (!is.null(add)) {
				add <- fun(add, na.rm=na.rm)
				x <- cbind(getValues(x), add)
			} else {
				x <- getValues(x)
			}
			x <- apply(x, 1, FUN=fun, na.rm=na.rm)
			out <- setValues(out, x)
			return(out)
		}
		
		tr <- blockSize(x)
		out <- writeStart(out, filename="")
		x <- readStart(x)

		pb <- pbCreate(tr$n)
		if (!is.null(add)) {
			add <- fun(add, na.rm=na.rm)
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				v <- apply(cbind(v, add), 1, FUN=fun, na.rm=na.rm)
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i) 
			} 
		} else {
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				v <- apply(v, 1, FUN=fun, na.rm=na.rm)
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i) 
			} 
		}
		pbClose(pb)			
		x <- readStop(x)
		writeStop(out)
	}
)


