# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3



if (!isGeneric("extent")) {
	setGeneric("extent", function(x, ...)
		standardGeneric("extent"))
}	

setMethod('extent', signature(x='Extent'), 
	function(x){ return(x) }
)

setMethod('extent', signature(x='BasicRaster'), 
	function(x, r1, r2, c1, c2){ 
		e <- x@extent
		r <- res(x)
		if (! missing(c1) )  { 
			xn <- xFromCol(x, c1) - 0.5 * r[1]
			if (is.na(xn)) {
				warning('invalid first colummn')
				xn <- e@xmin
			}
		} else { 
			xn <- e@xmin 
		}
		if (! missing(c2) )  { 
			xx <- xFromCol(x, c2) + 0.5 * r[1]
			if (is.na(xx)) {
				warning('invalid second colummn')
				xx <- e@xmax
			}
		} else {
			xx <- e@xmax 
		}
		if (! missing(r1) )  { 
			yx <- yFromRow(x, r1) + 0.5 * r[2]
			if (is.na(yx)) {
				warning('invalid first row')
				yx <- e@ymax
			}
		} else {
			yx <- e@ymax 
		}
		if (! missing(r2) )  {
			yn <- yFromRow(x, r2) - 0.5 * r[2]
			if (is.na(yn)) {
				warning('invalid second row')			
				yn <- e@ymin
			}
		} else { 
			yn <- e@ymin 
		}
		if (xn == xx) {
			stop('min and max x are the same')
		}
		if (yn == yx) {
			stop('min and max y are the same')
		}
		if (xn > xx) {
			warning('min x larger than max x')
		}
		if (yn > yx) {
			warning('min y larger than max y')
		}
		
		e <- extent(sort(c(xn, xx)), sort(c(yn, yx)))
		if (methods::validObject(e)) { 
			return(e) 
		}
	}
)

setMethod('extent', signature(x='Spatial'), 
	function(x){ 
		bndbox <- bbox(x)
		e <- methods::new('Extent')
		e@xmin <- bndbox[1,1]
		e@xmax <- bndbox[1,2]
		e@ymin <- bndbox[2,1]
		e@ymax <- bndbox[2,2]
		return(e) 
	}
)

setMethod('extent', signature(x='sf'), 
	function(x){ 
		if (!requireNamespace("sf")) {
			stop('Cannot do this because sf is not available')
		}
		b <- sf::st_bbox(x)
		e <- methods::new('Extent')
		e@xmin <- b[1]
		e@xmax <- b[3]
		e@ymin <- b[2]
		e@ymax <- b[4]
		return(e) 
	}
)


setMethod('extent', signature(x='matrix'), 
	function(x){ 
		d <- dim(x)
		if (min(d) < 2) {
			stop('matrix should have dimensions of at least 2 by 2') }		
		if (d[2] > 2) {
			stop('matrix should not have more than 2 columns') }		
		e <- methods::new('Extent')
		if (nrow(x) == 2) {
		# assuming a 'sp' bbox object
			e@xmin <- min(x[1,])
			e@xmax <- max(x[1,])
			e@ymin <- min(x[2,])
			e@ymax <- max(x[2,])
		} else {
			a <- as.vector(apply(x, 2, range, na.rm=TRUE))
			e@xmin <- a[1]
			e@xmax <- a[2]
			e@ymin <- a[3]
			e@ymax <- a[4]
		}
		if (validObject(e)) return(e)
	}
)
	
setMethod('extent', signature(x='numeric'), 
	function(x, ...){ 
		dots <- unlist(list(...))
		x <- c(x, dots)
		if (length(x) < 4) {
			stop('insufficient number of elements (should be 4)')
		}
		if (length(x) > 4) {
			warning('more elements than expected (should be 4)')
		}
		names(x) <- NULL
		e <- methods::new('Extent')
		e@xmin <- x[1]
		e@xmax <- x[2]
		e@ymin <- x[3]
		e@ymax <- x[4]
		if (validObject(e)) return(e)
	}	
)


# contributed by Etienne Racine
setMethod('extent', signature(x='list'),
	function(x, ...) {
		stopifnot(c("x", "y") %in% names(x))
		stopifnot(lapply(x[c("x", "y")], length) >= 2)
		lim <- c(range(x$x), (range(x$y)))
		return(extent(lim,...))
	}
)



setMethod('extent', signature(x='GridTopology'),
# contributed by Michael Sumner
	function(x){
		cco <- x@cellcentre.offset
		cs <- x@cellsize
		cdim <- x@cells.dim
		e <- methods::new('Extent')
		e@xmin <- cco[1] - cs[1]/2
		e@xmax <- e@xmin + cs[1] * cdim[1]
		e@ymin <- cco[2] - cs[2]/2
		e@ymax <- e@ymin + cs[2] * cdim[2]
		return(e)
    }
)


setMethod("[", c("Extent", "numeric", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	x <- as.vector(x)
	i <- as.integer(i)
	i <- i[i %in% 1:4]
	x[i]
})

setMethod("[", c("Extent", "missing", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	as.vector(x)
})

setReplaceMethod("[", c("Extent","numeric","missing"),
	function(x, i, j, value) {
		i <- as.integer(i)
		i <- i[i %in% 1:4]
		if (length(i) == 0) {
			return(x)
		}
		y <- as.vector(x)
		y[i] <- value
		if (y[1] >= y[2]) {
			stop('invalid extent. xmin should be greater than xmax')
		}
		if (y[3] >= y[4]) {
			stop('invalid extent. ymin should be greater than ymax')
		}
		x@xmin <- y[1]
		x@xmax <- y[2]
		x@ymin <- y[3]
		x@ymax <- y[4]
		return(x)
	}
)
