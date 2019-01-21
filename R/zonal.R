# Author: Robert J. Hijmans
# Date : March 2009
# Version 0.9
# Licence GPL v3



setMethod('zonal', signature(x='RasterLayer', z='RasterLayer'), 
	function(x, z, fun='mean', digits=0, na.rm=TRUE, ...) {
	
		# backward compatibility
		if (!is.null(list(...)$stat)) {
			stop('argument "stat" was replaced by "fun"')
		} 

		compareRaster(c(x, z))
		stopifnot(hasValues(z))
		stopifnot(hasValues(x))
	
		layernames <- names(x)
	
		if (canProcessInMemory(x, 3)) {
			inmem <- TRUE
		} else {
			inmem <- FALSE
		}
	

		if (inmem) {
			pb <- pbCreate(2, label='zonal', ...)		
			if (isTRUE(try(fun == 'count', silent=TRUE))) {
				func <- function(x, na.rm) {
					if (na.rm) {
						length(stats::na.omit(x))
					} else {
						length(x)
					}
				}		
			} else {
				func <- match.fun(fun)
			}	
			x <- getValues(x)
			z <- round(getValues(z), digits=digits)
			pb <- pbStep(pb, 1)		
			alltab <- tapply(x, z, FUN=func, na.rm=na.rm) 
			
			if (is.array(alltab)) { # multiple numbers
				id <- as.numeric(dimnames(alltab)[[1]])
				alltab <- matrix(unlist(alltab, use.names = FALSE), nrow=dim(alltab), byrow=TRUE)
				alltab <- cbind(id, alltab)
			} else {
				alltab <- cbind(as.numeric(names(alltab)), alltab)
			}
			pb <- pbStep(pb, 2)
			colnames(alltab)[1] <- 'zone'
			d <- dim(alltab)[2]
			if (d==2) {
				if (is.character(fun)) {
					colnames(alltab)[2] <- fun[1]
				} else {
					colnames(alltab)[2] <- 'value'
				}
			} else {
				colnames(alltab)[2:d] <- paste0('value_', 1:(d-1))
			}
			
		} else {
		
			if (class(fun) != 'character') {
				stop("RasterLayers cannot be processed in memory.\n You can use fun='sum', 'mean', 'sd', 'min', 'max', or 'count' but not a function")
			}
			if (! fun %in% c('sum', 'mean', 'sd', 'min', 'max', 'count')) {
				stop("fun can be 'sum', 'mean', 'sd', 'min', 'max', or 'count'")
			}
			sdtab <- FALSE
			counts <- FALSE		
			if (fun == 'count') {
				func1 <- function(x, na.rm) {
					if (na.rm) {
						length(stats::na.omit(x))
					} else {
						length(x)
					}
				}	
				func2 <- sum
			} else {
				func1 <- func2 <- match.fun(fun)
			}
			if ( fun == 'mean' | fun == 'sd') {
				func1 <- func2 <- sum
				counts <- TRUE
				if (fun == 'sd') {
					sdtab <- TRUE
				}
			} 

			alltab <- array(dim=0)
			sqtab <- cnttab <- alltab
	
			tr <- blockSize(x, n=2)
			pb <- pbCreate(tr$n, label='zonal', ...)
		
			#nc <- nlayers(x)
			#nc1 <- nc + 1
			#nc2 <- 2:nc1
			#nc2 <- 2
			x <- readStart(x, ...)
			z <- readStart(z, ...)
			
			for (i in 1:tr$n) {
				d <- cbind(getValues(x, row=tr$row[i], nrows=tr$nrows[i]))
				Z <- round(getValues(z, row=tr$row[i], nrows=tr$nrows[i]), digits=digits)
				#cat(i, '\n')
				#utils::flush.console()
				
				a <- tapply(d, Z, FUN=func1, na.rm=na.rm)
				a <- cbind(as.numeric(names(a)), a)
				alltab <- rbind(alltab, a) 
				if (counts) {
					if (na.rm) {
						a <- tapply(d, Z, FUN=function(x)length(stats::na.omit(x)))
						a <- cbind(as.numeric(names(a)), a)
						cnttab <- rbind(cnttab, a)
						if (sdtab) {
							a <- tapply( d^2, Z, FUN=function(x)sum(stats::na.omit(x)))
							a <- cbind(as.numeric(names(a)), a)
							sqtab <- rbind(sqtab, a)
						}
					} else {
						a <- tapply(d, Z, FUN=length)
						a <- cbind(as.numeric(names(a)), a)
						cnttab <- rbind(cnttab, a)
						if (sdtab) {
							a <- tapply(d^2, Z, FUN=sum)
							a <- cbind(as.numeric(names(a)), a)
							sqtab <- rbind(sqtab, a)
						}
					}
				}
				if (length(alltab) > 10000) {
					alltab <- tapply(alltab[,2], alltab[,1], FUN=func2, na.rm=na.rm) 
					alltab <- cbind(as.numeric(names(alltab)), alltab)
					if (counts) {
						cnttab <- tapply(cnttab[,2], cnttab[,1], FUN=sum, na.rm=na.rm) 
						cnttab <- cbind(as.numeric(names(cnttab)), cnttab)
						if (sdtab) {
							sqtab <- tapply(sqtab[,2], sqtab[,1], FUN=sum, na.rm=na.rm) 
							sqtab <- cbind(as.numeric(names(sqtab)), sqtab)
						}
					}
				}
				pbStep(pb, i)
			}
			x <- readStop(x)
			z <- readStop(z)
			
			alltab <- tapply(alltab[,2], alltab[,1], FUN=func2, na.rm=na.rm)
			alltab <- cbind(as.numeric(names(alltab)), alltab)
			if (counts) {
				cnttab <- tapply(cnttab[,2], cnttab[,1], FUN=sum) 
				cnttab <- cbind(as.numeric(names(cnttab)), cnttab)
				alltab[,2] <- alltab[,2] / cnttab[,2]
				if (sdtab) {
					sqtab <- tapply(sqtab[,2], sqtab[,1], FUN=sum, na.rm=na.rm) 
					sqtab <- cbind(as.numeric(names(sqtab)), sqtab)
					alltab[,2] <- sqrt(( (sqtab[,2] / cnttab[,2]) - (alltab[,2])^2 ) * (cnttab[,2]/(cnttab[,2]-1)))
				}
				
			}
			colnames(alltab)[1] <- 'zone'
			if (is.character(fun)) {
				colnames(alltab)[2] <- fun
			} else {
				colnames(alltab)[2] <- 'value'
			}		
		}
		#alltab <- as.matrix(alltab)
		pbClose(pb)
		return(alltab)
	}
)

#zonal(r, z, 'sd')




setMethod('zonal', signature(x='RasterStackBrick', z='RasterLayer'), 
	function(x, z, fun='mean', digits=0, na.rm=TRUE, ...) {

		# backward compatibility
		if (!is.null(list(...)$stat)) {
			stop('argument "stat" was replaced by "fun"')
		} 
	
		compareRaster(c(x, z))
		stopifnot(hasValues(z))
		stopifnot(hasValues(x))
	
		layernames <- names(x)
	
		if (canProcessInMemory(x, 3)) {
			inmem <- TRUE
		} else {
			inmem <- FALSE
		}
	
		if (inmem) {
			pb <- pbCreate(2, label='zonal', ...)		
			if (isTRUE(try(fun == 'count', silent=TRUE))) {
				func <- function(x, na.rm) {
					if (na.rm) {
						length(stats::na.omit(x))
					} else {
						length(x)
					}
				}		
			} else {
				func <- match.fun(fun)
			}	
			
			x <- getValues(x)
			x <- cbind(x, round(getValues(z), digits=digits))
			pb <- pbStep(pb, 1)		
			alltab <- aggregate(x[,1:(ncol(x)-1)], by=list(x[,ncol(x)]), FUN=func, na.rm=na.rm) 
			fun <- 'value'
			pb <- pbStep(pb, 2)
			
		} else {
		
			if (class(fun) != 'character') {
				stop("RasterLayers cannot be processed in memory.\n You can use fun='sum', 'mean', 'sd', 'min', 'max', or 'count' but not a function")
			}
			if (! fun %in% c('sum', 'mean', 'sd', 'min', 'max', 'count')) {
				stop("fun can be 'sum', 'mean', 'sd', 'min', 'max', or 'count'")
			}
			sdtab <- FALSE
			counts <- FALSE		
			
			if (fun == 'count') {
				func1 <- function(x, na.rm) {
					if (na.rm) {
						length(stats::na.omit(x))
					} else {
						length(x)
					}
				}	
				func2 <- sum
			} else {
				func1 <- func2 <- match.fun(fun)
			}
			if ( fun == 'mean' | fun == 'sd') {
				func1 <- func2 <- sum
				counts <- TRUE
				if (fun == 'sd') {
					sdtab <- TRUE
				}
			} 

			alltab <- array(dim=0)
			sqtab <- cnttab <- alltab
	
			tr <- blockSize(x, n=2)
			pb <- pbCreate(tr$n, label='zonal', ...)
		
			nc <- nlayers(x)
			nc1 <- nc + 1
			nc2 <- 2:nc1
			
			# for a RasterStack it would be more efficient to loop over the layers
			x <- readStart(x, ...)
			z <- readStart(z, ...)
			
			for (i in 1:tr$n) {
				d <- cbind(getValues(x, row=tr$row[i], nrows=tr$nrows[i]),   
					 round(getValues(z, row=tr$row[i], nrows=tr$nrows[i]), digits=digits))
				#cat(i, '\n')
				#utils::flush.console()
				alltab <- rbind(alltab, aggregate(d[,1:nc], by=list(d[,nc1]), FUN=func1, na.rm=na.rm)) 
				if (counts) {
					if (na.rm) {
						cnttab <- rbind(cnttab, aggregate(d[,1:nc], by=list(d[,nc1]), FUN=function(x)length(stats::na.omit(x))))
						if (sdtab) {
							sqtab <- rbind(sqtab, aggregate( (d[,1:nc])^2, by=list(d[,nc1]), FUN=function(x)sum(stats::na.omit(x))))
						}
					} else {
						cnttab <- rbind(cnttab, aggregate(d[,1:nc], by=list(d[,nc1]), FUN=length))				
						if (sdtab) {
							sqtab <- rbind(sqtab, aggregate( (d[,1:nc])^2, by=list(d[,nc]), FUN=sum))
						}
					}
				}
				if (length(alltab) > 10000) {
					alltab <- aggregate(alltab[,nc2], by=list(alltab[,1]), FUN=func2, na.rm=na.rm) 
					if (counts) {
						cnttab <- aggregate(cnttab[,nc2], by=list(cnttab[,1]), FUN=sum, na.rm=na.rm) 
						if (sdtab) {
							sqtab <- aggregate(sqtab[,nc2], by=list(sqtab[,1]), FUN=sum, na.rm=na.rm) 
						}
					}
				}
				pbStep(pb, i)
			}
			x <- readStop(x)
			z <- readStop(z)
			
			alltab <- aggregate(alltab[,nc2], by=list(alltab[,1]), FUN=func2, na.rm=na.rm) 	
			if (counts) {
				cnttab <- aggregate(cnttab[,nc2], by=list(cnttab[,1]), FUN=sum) 
				alltab[,nc2] <- alltab[,nc2] / cnttab[,nc2]
				if (sdtab) {
					sqtab <- aggregate(sqtab[,nc2], by=list(sqtab[,1]), FUN=sum, na.rm=na.rm) 
					alltab[,nc2] <- sqrt(( (sqtab[,nc2] / cnttab[,nc2]) - (alltab[nc2])^2 ) * (cnttab[,nc2]/(cnttab[,nc2]-1)))
				}
				
			}
			
		}
	
		alltab <- as.matrix(alltab)
		colnames(alltab)[1] <- 'zone'
		if (ncol(alltab) > 2) {
			colnames(alltab)[2:ncol(alltab)] <- layernames
		} else {
			colnames(alltab)[2] <- fun[1]
		}
		pbClose(pb)
	
		return(alltab)
	}
)

#zonal(r, z, 'sd')


