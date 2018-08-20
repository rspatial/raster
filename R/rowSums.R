# Author: Robert J. Hijmans
# Date : April 2015
# Version 1.0
# Licence GPL v3


setMethod('rowSums', signature(x='Raster'), 
	function(x,  na.rm = FALSE, dims = 1L, ...) {
		nl <- nlayers(x)
		if (canProcessInMemory(x)) {
			if(nl == 1) {
				# colSums because of row-wise Raster objects and col-wise R matrices and 
				return(.colSums(getValues(x), ncol(x), nrow(x), na.rm=na.rm, ...))
			} else {
				r <- .colSums(getValues(x), ncol(x), nrow(x)*nl, na.rm=na.rm, ...)
				r <- matrix(r, ncol=nl)
				colnames(r) <- names(x)
				return(r)
			}
		} else {
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, label='rowSums', ...)
			nc <- ncol(x)
			if(nl == 1) {
				s <- list()
				for (i in 1:tr$n) {
					v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
					s[[i]] <- .colSums(v, nc, tr$nrows[i], na.rm=na.rm, ...)
				}
				return(unlist(s, use.names = FALSE))			
			} else {
				s <- list()
				for (i in 1:tr$n) {
					v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
					s[[i]] <- .colSums(v, nc, tr$nrows[i]*nl, na.rm=na.rm, ...)
				}
				s <- t(matrix(unlist(s), nrow=nl))
				colnames(s) <- names(x)
				return(s)
			}
		}
	}
)





setMethod('colSums', signature(x='Raster'), 
	function(x, na.rm = FALSE, dims = 1L, ...) {
		nl <- nlayers(x)
		if (canProcessInMemory(x)) {
			if(nl == 1) {
				return(.colSums(as.matrix(x), nrow(x), ncol(x), na.rm=na.rm, ...))
			} else {
				r <- getValues(x)
				s <- list()
				nc <- ncol(x)
				nr <- nrow(x)
				for (i in 1:nl) {
					v <- matrix(r[,i], nrow=nc)
					s[[i]] <- .rowSums(v, nc, nr, na.rm=na.rm, ...)
				}
				s <- matrix(unlist(s), ncol=nl)
				colnames(s) <- names(x)
				return(s)
			}
		} else {
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, label='colSums', ...)
			nc <- ncol(x)
			if(nl == 1) {
				s <- list()
				for (i in 1:tr$n) {
					v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
					s[[i]] <- .colSums(matrix(v, nrow=tr$nrows[i], byrow=TRUE), tr$nrows[i], nc, na.rm=na.rm, ...)
				}
				s <- colSums(matrix(unlist(s), nrow=tr$n, byrow=T))
				return(s)
			} else {

				s <- matrix(nrow=tr$n, ncol=nc*nl) 
				for (i in 1:tr$n) {
					v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
					for (j in 1:nl) {
						k <- (j-1) * nc + 1
						k <- k:(k+nc-1)
						s[i, k] <- .colSums(matrix(v[,j], nrow=tr$nrows[i], byrow=TRUE), tr$nrows[i], nc, na.rm=na.rm, ...)
					}
				}
				s <- matrix(.colSums(s, nrow(s), ncol(s), na.rm=na.rm), ncol=nl)
				colnames(s) <- names(x)
				return(s)
			}
		}
	}
)
