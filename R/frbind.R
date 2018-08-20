# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3

# friendly rbind
# rbinds data.frames with different column names
.frbind <- function(x, ...) {

	if (! inherits(x, 'data.frame') ) {
		x <- data.frame(x)
	}

	d <- list(...)
	if (length(d) == 0) { return(x) }
	
	for (i in 1:length(d)) {
		
		dd <- d[[i]]
		if (! inherits(dd, 'data.frame')) {
			dd <- data.frame(dd)
		}
		
		cnx <- colnames(x)
		cnd <- colnames(dd)
		
		e <- cnx[(cnx %in% cnd)]	
		for (j in e) {
			if (class(x[,j]) != class(dd[,j])) {
				x[,j] <- as.character(x[,j])
				dd[,j] <- as.character(dd[,j])
			}
		}
		
		a <- which(!cnd %in% cnx)
		if (length(a) > 0) {
			zz <- dd[NULL, a, drop=FALSE]
			zz[1:nrow(x),] <- NA
			x <- cbind(x, zz)
		}

		b <- which(!cnx %in% cnd)
		if (length(b) > 0) {
			zz <- x[NULL, b, drop=FALSE]
			zz[1:nrow(dd),] <- NA
			dd <- cbind(dd, zz)
		}
		
		x <- rbind(x, dd)		
	}
	x
}


.frbindMatrix <- function(x, ...) {

	d <- list(...)
	if (length(d) == 0) { return(x) }
	
	for (i in 1:length(d)) {
		
		dd <- d[[i]]
		
		cnx <- colnames(x)
		cnd <- colnames(dd)
		
		a <- which(!cnd %in% cnx)
		if (length(a) > 0) {
			zz <- dd[NULL, a, drop=FALSE]
			zz[1:nrow(x),] <- NA
			x <- cbind(x, zz)
		}

		b <- which(!cnx %in% cnd)
		if (length(b) > 0) {
			zz <- x[NULL, b, drop=FALSE]
			zz[1:nrow(dd),] <- NA
			dd <- cbind(dd, zz)
		}
		
		x <- rbind(x, dd)		
	}
	x
}
