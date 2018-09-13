# Author: Robert J. Hijmans
# Date : December 2009
# Version 1.0
# Licence GPL v3



setMethod('trim', signature(x='character'), 
	function(x, internal=FALSE, ...) {
		if (internal) {
			gsub("^ *|(?<= ) | *$", "", x, perl=TRUE)
		} else {
			gsub("^\\s+|\\s+$", "", x)
		}
	}
)


setMethod('trim', signature(x='data.frame'), 
	function(x, ...) {
		for (i in 1:ncol(x)) {
			if (class(x[,i]) == 'character') {
				x[,i] <- trim(x[,i])
			} else if (class(x[,i]) == 'factor') {
				x[,i] <- as.factor(trim(as.character(x[,i])))
			}	
		}
		return(x)
	}
)


setMethod('trim', signature(x='matrix'), 
	function(x, ...) {
		if (is.character(x)) {
			x[] = trim(as.vector(x))
		} else {
			rows <- rowSums(is.na(x))
			cols <- colSums(is.na(x))
			rows <- which(rows != ncol(x))
			cols <- which(cols != nrow(x))
			if (length(rows)==0) {
				x <- matrix(ncol=0, nrow=0)
			} else {
				x <- x[min(rows):max(rows), min(cols):max(cols), drop=FALSE]
			}
		}
		return(x)
	}
)



# June 2013, modification by Mike Sumner, added argument "value"


.memtrimlayer <- function(r, padding=0, values=NA, filename='', ...) {
	x <- as.matrix(r)
	if (all(is.na(values))) {
		rows <- rowSums(is.na(x))
		cols <- colSums(is.na(x))
	} else {
		rows <- apply(x, 1, function(i) sum(i %in% values))
		cols <- apply(x, 2, function(i) sum(i %in% values))
	}
	rows <- which(rows != ncol(x))
	cols <- which(cols != nrow(x))
	if (length(rows)==0) { 	stop('only NA values found') }
	
	rows <- pmin(pmax(1, c(min(rows) - padding, max(rows + padding))), nrow(r))
	cols <- pmin(pmax(1, c(min(cols) - padding, max(cols + padding))), ncol(r))

	r[rows[1]:rows[2], cols[1]:cols[2], drop=FALSE]
}


setMethod('trim', signature(x='Raster'), 
function(x, padding=0, values=NA, filename='', ...) {

	filename <- trim(filename)
	if (!.hasValues(x)) { stop('The Raster object has no values') } 
	
	if (nlayers(x) == 1 && canProcessInMemory(x)) {
		x <- .memtrimlayer(x, padding=padding, ...) 
		if (filename != '') {
			return(writeRaster(x, filename=filename, ...))
		} else {
			return(x)
		}
	}
	
	nr <- nrow(x)
	nc <- ncol(x)
	nrl <- nr * nlayers(x)
	ncl <- nc * nlayers(x)
	
	cnt <- 0

	for (r in 1:nr) {
		v <- getValues(x, r)
		if (sum(v %in% values) < ncl) {
			break 
		}
		cnt <- cnt + 1
	}
	if ( cnt == nr) { stop('only NA values found') }
	firstrow <- min(max(r-padding, 1), nr)
	
	for (r in nr:firstrow) {
		v <- getValues(x, r)
		if (sum(v %in% values) < ncl) { 
			break 
		}
	}
	lastrow <- max(min(r+padding, nr), 1)
	
	if (lastrow < firstrow) { 
		tmp <- firstrow
		firstrow <- lastrow
		lastrow <- tmp
	}
	
	for (c in 1:nc) {
		v <- getValuesBlock(x, 1 ,nrow(x), c, 1)
		if (sum(v %in% values) < nrl) { break }
	}
	firstcol <- min(max(c-padding, 1), nc) 
	
	for (c in nc:firstcol) {
		v <- getValuesBlock(x, 1 ,nrow(x), c, 1)
		if (sum(v %in% values) < nrl) { break }
	}
	lastcol <- max(min(c+padding, nc), 1)
	
	if (lastcol < firstcol) { 
		tmp <- firstcol
		firstcol <- lastcol
		lastcol <- tmp
	}
	
	xr <- xres(x)
	yr <- yres(x)
	e <- extent(xFromCol(x, firstcol)-0.5*xr, xFromCol(x, lastcol)+0.5*xr, yFromRow(x, lastrow)-0.5*yr, yFromRow(x, firstrow)+0.5*yr)
	
	return( crop(x, e, filename=filename, ...) )
}
)

