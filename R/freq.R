# Author: Robert J. Hijmans
# Date : March 2009
# Version 0.9
# Licence GPL v3




setMethod('freq', signature(x='RasterLayer'), 
	function(x, digits=0, value=NULL, useNA="ifany", progress='', ...) {
		
		if (!is.null(value)) {
			return( .count(x, value, digits=digits, progress=progress, ...) )
		}
	
		if (canProcessInMemory(x, 3)) {
	
			d <- round(getValues(x), digits=digits)
			res <- table( d, useNA=useNA )
			res <- cbind(as.numeric(names(res)), as.vector(res))
		
		} else {
		
			tr <- blockSize(x, n=2)
			pb <- pbCreate(tr$n, progress=progress, label='freq')	
			z <- vector(length=0)
			for (i in 1:tr$n) {
				d <- round(getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i]), digits=digits)
				res <- table(d, useNA=useNA )
				res <- cbind(as.numeric(unlist(as.vector(dimnames(res)), use.names = FALSE)), as.vector(res))
				z <- rbind(z, res)
				pbStep(pb, i)
			}
			res <- tapply(z[,2], as.character(z[,1]), sum)	
			res <- cbind(as.numeric(names(res)), as.vector(res))
			z <- z[is.na(z[,1]), ,drop=FALSE]
			if (isTRUE(nrow(z) > 0)) {
				z <- sum(z[,2])
				res <- rbind(res, c(NA, z))
			}
			res <- res[order(res[,1]), ]
			pbClose(pb)	
		}
	
		colnames(res) <- c('value', 'count')
		return(res)
	}
)



setMethod('freq', signature(x='RasterStackBrick'), 
	function(x, digits=0, value=NULL, useNA="ifany", merge=FALSE, progress='', ...) {

		if (!is.null(value)) {
			return(.count(x, value, digits=digits, progress=progress, ...))
		}
	
		nl <- nlayers(x)
		res <- list()
		
		pb <- pbCreate(nl, progress=progress, label='freq')	
		for (i in 1:nl) { 
			res[[i]] <- freq( raster(x, i), digits=digits, useNA=useNA, progress='', ...) 
			pbStep(pb, i)
		}
		pbClose(pb)
		
		names(res) <- ln <- names(x)
		
		if (merge) {
			r <- res[[1]]
			colnames(r)[2] <- ln[1]
			if (nl > 1) {			
				for (i in 2:nl) {
					x <- res[[i]]
					colnames(x)[2] <- ln[i]
					r <- merge(r, x, by=1, all=TRUE)
				}
			}
			return(r)
		}
		
		return(res)
	}
)



.count <- function(x, value, digits=0, progress='', ...) {

	value <- value[1]
	
	if (nlayers(x) > 1) {
	
		if (canProcessInMemory(x, 2)) {
			if (is.na(value)) {
				v <-  colSums(is.na(getValues(x)))
			} else {
				v <- round(getValues(x), digits=digits) == value
				v <- colSums(v, na.rm=TRUE)
			}
		} else {
			tr <- blockSize(x, n=2)
			pb <- pbCreate(tr$n, progress=progress)
			v <- 0
			for (i in 1:tr$n) {
				vv <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				if (is.na(value)) {
					v <- v + colSums(is.na(vv))
				} else {
					vv <- round(v, digits=digits) == value
					v <- v + colSums(vv, na.rm=TRUE)
				}
				pbStep(pb, i)
			}
			pbClose(pb)
		}
		return(v)	
	
	} else {
	
		if (canProcessInMemory(x, 2)) {
			if (is.na(value)) {
				x <- sum(is.na(getValues(x)))
			} else {
				v <- stats::na.omit(round(getValues(x), digits=digits))
				x <- sum(v == value)
			}
			return(x)
		} else {
			tr <- blockSize(x, n=2)
			pb <- pbCreate(tr$n, progress=progress)
			r <- 0
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				if (is.na(value)) {
					r <- r + sum(is.na(v))
				} else {
					v <- stats::na.omit(round(v, digits=digits))
					r <- r + sum(v == value)
				}
				pbStep(pb, i)
			}
			pbClose(pb)
			return(r)
		}
	}
}

