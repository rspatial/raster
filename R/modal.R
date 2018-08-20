# Author: Robert J. Hijmans 
# Date :  October 2008
# revised: October 2011, May 2015
# Version 1.0
# Licence GPL v3


setGeneric("modal", function(x, ...)
	standardGeneric("modal"))
	
	
setMethod('modal', signature(x='ANY'), 
function(x, ..., ties='random', na.rm=FALSE, freq=FALSE) {

	dots <- list(...)
	if ( length(dots) > 0 ) {
		# change fact to char because 
		# c(x, ...) would change it to integers
		# and levels would a mess too with multiple objects
		if (is.factor(x)) {
			x <- as.character(x)
			dots <- unlist(lapply(dots, as.character))
		}
		x <- c(x, unlist(dots))
	}

	
	# NA itself cannot be the modal value
	# perhaps that should be allowed as an option
	z <- x[!is.na(x)]
	if (length(z) == 0) { 
		return(NA) 
	} else if (!na.rm & length(z) < length(x)) { 
		return(NA)	 
	}

	if (freq) {
		if (length(z) == 1) {
			return(1)
		} else {
			return(max( table(z) ))
		}
	}  	

	ties <- match(ties[1], c('lowest', 'highest', 'first', 'random', 'NA')) - 1
	if (is.na(ties)) {
		stop("the value of 'ties' should be 'lowest', 'highest', 'first', 'random' or 'NA'")
	}
		
	if (length(z) == 1) {
		return(z)
	} else if (is.numeric(z)) {
		w <- .getMode(z, ties=ties)
	} else if (is.logical(z)) {
		w <- as.logical(.getMode(z, ties=ties))
	} else if (is.factor(z)) {  
		w <- .getMode(z, ties=ties)
		w <- levels(z)[w]
		w <- factor(w, levels=levels(z))
	} else { #  character, perhaps others?
		z <- as.factor(z)
		w <- .getMode(z, ties=ties)
		w <- levels(z)[w]
	}
	return(w)
}
)

