# author Robert Hijmans
# June 2010
# version 1.0
# license GPL3

.compareCRS <- function(...) {
	warning('use "compareCRS", not ".compareCRS"')
	compareCRS(...)
}

compareCRS <- function(x, y, unknown=FALSE, verbatim=FALSE, verbose=FALSE) {
	
	x <- tolower(projection(x))
	y <- tolower(projection(y))
	
	step1 <- function(z) {
		z <- gsub(' ', '', z)
		if (!verbatim) {
			z <- unlist( strsplit(z, '+', fixed=TRUE) )[-1]
			z <- do.call(rbind, strsplit(z, '='))
		}
		z
	}
	
	if (verbatim) {
		if (!is.na(x) & !is.na(y)) {
			return(x==y)
		} else {
			if (is.na(x) & is.na(y)) {
				return(TRUE) # ??

			} else if (unknown) {
				return(TRUE) 
			} else {
				return(FALSE) 			
			}
		}
	}

	x <- step1(x)
	y <- step1(y)

	if (length(x) == 0 & length(y) == 0) {
		return(TRUE)
	} else if (length(x) == 0 | length(y) == 0) {
		if (unknown) {
			return(TRUE)
		} else {
			if (verbose) {
				message('Unknown CRS')
			}
			return(FALSE) 
		}
	}
	x <- x[x[,1] != 'towgs84', , drop=FALSE]
	x <- x[x[,1] != 'no_defs', , drop=FALSE]
	x <- x[which(x[,1] %in% y[,1]), ,drop=FALSE]
	y <- y[which(y[,1] %in% x[,1]), ,drop=FALSE]
	x <- x[order(x[,1]), ,drop=FALSE]
	y <- y[order(y[,1]), ,drop=FALSE]
	i <- x[,2] == y[,2]
	
	if (! all(i)) {
		if (verbose) {
			i <- which(!i)
			for (j in i) {
				message('+',x[j,1], ':  ', x[j,2],' != ', y[j,2], '\n')
			}
		}
		return(FALSE)
	}
	return(TRUE)
}


