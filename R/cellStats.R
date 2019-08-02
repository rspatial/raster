# Author: Robert J. Hijmans
# Date : March 2009 / April 2012
# Version 1.0
# Licence GPL v3



.csTextFun <- function(fun) {
	if (class(fun) != 'character') {
		if (is.primitive(fun)) {
			test <- try(deparse(fun)[[1]], silent=TRUE)
			if (test == '.Primitive(\"sum\")') { fun <- 'sum' 
			} else if (test == '.Primitive(\"min\")') { fun <- 'min' 
			} else if (test == '.Primitive(\"max\")') { fun <- 'max' 
			}
		} else {
			f <- paste(deparse(fun), collapse = "\n")
			if (f == paste(deparse(mean), collapse = "\n")) {
				fun <- 'mean' 
			} else if (f == paste(deparse(stats::sd), collapse = "\n")) {
				fun <- 'sd' 
			} else if (f == paste(deparse(range), collapse = "\n")) {
				fun <- 'range' 
			} 			
		} 
	}
	return(fun)
}


	
if (!isGeneric("cellStats")) {
	setGeneric("cellStats", function(x, stat, ...)
		standardGeneric("cellStats"))
}	


setMethod('cellStats', signature(x='RasterStackBrick'),
	function(x, stat='mean', na.rm=TRUE, asSample=TRUE, ...) {
	
		stopifnot(hasValues(x))

		makeMat <- FALSE
		if (nlayers(x) == 1) {	
			makeMat <- TRUE
			#return( cellStats(raster(x, values=TRUE, stat=stat, ...) )		
		}
	
		stat <- .csTextFun(stat)
	
		if (!inMemory(x)) {
			if (canProcessInMemory(x)) {
				x <- readAll(x)
			}
		}
		if (inMemory(x) ) {
			x <- getValues(x)
			if (makeMat) {
				x <- matrix(x, ncol=1)
			}

			if (class(stat) == 'character') {
				if (stat == "mean" ) {
					return( colMeans(x, na.rm=na.rm) )
			
				} else if (stat == "sum" ) {
					return( colSums(x, na.rm=na.rm) )

				} else if (stat == "min" ) {
					v <- .colMin(x, na.rm=na.rm) 
					names(v) <- names(x)
					return(v)

				} else if (stat == "max" ) {
					v <- .colMax(x, na.rm=na.rm)
					names(v) <- names(x)
					return(v)
					
				} else if (stat == 'countNA') { 
					warning ("'countNA' is deprecated. Use 'freq(x, value=NA)' instead")
					return( colSums(is.na(x)) )
				
				} else if (stat == 'sd') { 
					
					st <- apply(x, 2, stats::sd, na.rm=na.rm) 
					if (! asSample) {
						if (na.rm) {
							n <- colSums(! is.na(x))
						} else {
							n <- nrow(x)
						}
						st <- sqrt(st^2 * ((n-1)/n))						
					} 
					
					return(st)

				} else if (stat == 'rms') { 
					if (na.rm) {
						n <- colSums(! is.na(x))
					} else {
						n <- nrow(x)
					}
					if (asSample) {
						n <- n-1
					}
					# st <- apply(x, 2, function(x) sqrt(sum(x^2)/n))
					return(  sqrt( apply(x, 2, function(x) sum(x^2))/n ) )
					

				} else if (stat == 'skew') { 
					if (na.rm) {
						n <- colSums(! is.na(x))
					} else {
						n <- nrow(x)
					}
					if (asSample) {
						sdx <- apply(x, 2, stats::sd, na.rm=na.rm)
					} else {
						sdx <- apply(x, 2, function(x) sqrt(sum((x-mean(x, na.rm=na.rm))^2, na.rm=na.rm)/n))
					}
					return(  colSums(t(t(x) - colMeans(x, na.rm=na.rm))^3, na.rm=na.rm) / (n * sdx^3) )
				}
			} # else 
			
			return(apply(x, 2, stat, na.rm=na.rm, ...))
		}
		
		if (class(stat) != 'character') {
			stop('cannot use this function for large files')
		}
		
		st <- NULL
		counts <- FALSE
		if (stat == 'sum') {
			fun <- sum
			st <- 0	
		} else if (stat == 'min') {
			st <- Inf
		} else if (stat == 'max') {
			st <- -Inf
		} else if (stat == 'range') {
			fun <- range
		} else if (stat == 'countNA') {
			warning ("'countNA' is depracted. Use freq(x, 'value=NA') instead")
			st <- 0	
			counts <- TRUE
		} else if (stat == 'skew') {
			
			zmean <- cellStats(x, 'mean')
			cnt <- 0
			d3 <- 0
			sumsq <- 0
			counts <- TRUE
			
		} else if (stat == 'mean' | stat == 'sd' | stat == 'rms') {
			st <- 0	
			sumsq <- 0
			cnt <- 0
			counts <- TRUE
		
		} else { 
			stop("invalid 'stat'. Should be 'sum', 'min', 'max', 'sd', 'mean', 'rms', or 'skew'") 
		}

			
		tr <- blockSize(x)
		pb <- pbCreate(tr$n, label='cellStats', ...)
		
		for (i in 1:tr$n) {
			d <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			if (makeMat) {
				d <- matrix(d, ncol=1)
			}
			if (counts) {
				if (na.rm & stat != 'countNA') {
					nas <- colSums( is.na(d) )
					if (min(nas) == nrow(d)) { 
						next 
					}
					cells <- nrow(d) - nas
				} else {
					if (stat == 'countNA') {
						nas <- colSums( is.na(d) )
					} else {
						cells <- nrow(d)
					}
				}
			}
				
			if (stat=='mean') {
				st <- colSums(d, na.rm=na.rm) + st
				cnt <- cnt + cells
			
			} else if (stat=='sum') {
				st <- colSums(d, na.rm=na.rm) + st

			} else if (stat == 'sd') {
				st <- colSums(d, na.rm=na.rm) + st
				cnt <- cnt + cells
				sumsq <- colSums(d^2, na.rm=na.rm) + sumsq

			} else if (stat=='countNA') {
				st <- st + nas
					
			} else if (stat=='rms') {
			
				sumsq <- colSums(d^2, na.rm=TRUE) + sumsq
				cnt <- cnt + cells

			} else if (stat=='skew') {

				d <- t( t(d) - zmean )
				sumsq <- colSums(d^2, na.rm=TRUE) + sumsq
				d3 <- colSums(d^3, na.rm=TRUE) + d3
				cnt <- cnt + cells

			} else if (stat=='min') {
				tmp <- .colMin(d, na.rm=na.rm)
				st <- pmin(st, tmp, na.rm=na.rm)

			} else if (stat=='max') {
				tmp <- .colMax(d, na.rm=na.rm)
				st <- pmax(st, tmp, na.rm=na.rm)
				
			} else {
					# range
				st <- apply(rbind(d, st), 2, fun, na.rm=na.rm)
			}
				
			pbStep(pb, i) 
		}
			
			
		if (stat == 'sd') {
			meansq <- (st/cnt)^2
			st <- sqrt(( (sumsq / cnt) - meansq ) * (cnt/(cnt-1)))
			if (!asSample) {
				#st <- sqrt( st^2 * (cnt / (cnt-1)))
				st <- sqrt( st^2 * ((cnt-1) / cnt))						

				
			}
		} else if (stat == 'mean') {
			st <- st / cnt
		} else if (stat == 'rms') {
			if (asSample) {
				st <- sqrt(sumsq/(cnt-1))
			} else {
				st <- sqrt(sumsq/cnt)
			}

		} else if (stat == 'skew') {

			if (asSample) {
				stsd <- sqrt(sumsq/(cnt-1))^3
			} else {
				stsd <- sqrt(sumsq/cnt)^3
			}
			st <- d3 / (cnt*stsd)
		} else if (stat %in% c('min', 'max')) {
			names(st) <- names(x)
		}
		
		pbClose(pb)
		return(st)
	}
)






setMethod('cellStats', signature(x='RasterLayer'),
	function(x, stat='mean', na.rm=TRUE, asSample=TRUE, ...) {
	
		stopifnot(hasValues(x))
		stat <- .csTextFun(stat)
	
		if (! inMemory(x) ) {
			if (canProcessInMemory(x)) {
				x <- readAll(x)
			}
		}
		if (inMemory(x) ) {
			x <- getValues(x)

			if (class(stat) == 'character') {
				if (stat == "mean" ) {
					return( mean(x, na.rm=na.rm) )
				} else if (stat == "sum" ) {
					return( sum(as.double(x), na.rm=na.rm ) )
				} else if (stat == 'countNA') { 
					return( sum(is.na(x)) )
				} else if (stat == "range" ) {
					return( range(x, na.rm=na.rm) )
				} else if (stat == "min" ) {
					return( min(x, na.rm=na.rm) )
				} else if (stat == "max" ) {
					return( max(x, na.rm=na.rm) )
				} else if (stat == "sd" ) {
					st <- stats::sd(x, na.rm=na.rm)
					if (! asSample) {
						if (na.rm) {
							n <- length(stats::na.omit(x))
						} else {
							n <- length(x)
						}
						#st <- sqrt(st^2 * (n/(n-1)))
						st <- sqrt(st^2 * ((n-1)/n))						
						
					} 
					return(st)
				} else if (stat == 'rms') { 
					if (na.rm) {
						n <- sum(! is.na(x))
					} else {
						n <- length(x)
					}
					if (asSample) {
						n <- n-1
					}
					# st <- apply(x, 2, function(x) sqrt(sum(x^2)/n))
					return(  sqrt( sum(x^2)/n ) )
					
					
				} else if (stat == "skew" ) {
					if (na.rm) {
						x <- stats::na.omit(x)
					}
					if (asSample) {
						sdx <- stats::sd(x)
					} else {
						sdx <- sqrt(sum((x-mean(x))^2)/(length(x)))
					}
					return( sum( (x - mean(x))^3 ) / (length(x) * sdx^3) )
				}
			} else {
				return( stat(x, na.rm=na.rm) )
			}
		}
		
		
		if (class(stat) != 'character') {
			stop('cannot use this function for large files')
		}
		
		st <- NULL
		counts <- FALSE
		if (stat == 'sum') {
			fun <- sum
			st <- 0	
		} else if (stat == 'min') {
			fun <- min
		} else if (stat == 'max') {
			fun <- max
		} else if (stat == 'range') {
			fun <- range
		} else if (stat == 'countNA') {
			st <- 0	
			counts <- TRUE
			
		} else if (stat == 'skew') {
			zmean <- cellStats(x, 'mean')
			cnt <- 0
			sumsq <- 0
			d3 <- 0
			counts <- TRUE
			
		} else if (stat == 'mean' | stat == 'sd' | stat == 'rms') {
			st <- 0	
			sumsq <- 0
			cnt <- 0
			counts <- TRUE
		} else { 
			stop("invalid 'stat'. Should be sum, min, max, sd, mean, or 'countNA'") 
		}

			
		tr <- blockSize(x)
		pb <- pbCreate(tr$n, label='cellStats', ...)
		
		for (i in 1:tr$n) {
			d <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			if (counts) {
				if (na.rm & stat != 'countNA') {
					nas <- sum(is.na(d) )
					if (nas == length(d)) { # only NAs 
						next 
					}
					cells <- length(d) - nas
				} else {
					if (stat == 'countNA') {
						nas <- sum(is.na(d) )
					} else {
						cells <- length(d)
					}
				}
			}
				
			if (stat=='mean') {
				st <- sum(d, na.rm=na.rm) + st
				cnt <- cnt + cells
			
			} else if (stat=='sum') {
				st <- sum(as.double(d), na.rm=na.rm) + st

			} else if (stat == 'sd') {
				st <- sum(d, na.rm=na.rm) + st
				cnt <- cnt + cells
				sumsq <- sum( d^2 , na.rm=na.rm) + sumsq

			} else if (stat=='countNA') {
				st <- st + nas
					
			} else if (stat=='skew') {
				
				d <- (d - zmean)
				sumsq <- sum(d^2, na.rm=na.rm) + sumsq
				d3 <- sum(d^3, na.rm=na.rm) + d3
				cnt <- cnt + cells

			} else if (stat=='rms') {
				sumsq <- sum( d^2, na.rm=na.rm) + sumsq
				cnt <- cnt + cells
				
			} else {
				st <- fun(d, st, na.rm=na.rm)
			}
				
			pbStep(pb, i) 
		}
		pbClose(pb)			
			
		if (stat == 'sd') {
			meansq <- (st/cnt)^2
			st <- sqrt(( (sumsq / cnt) - meansq ) * (cnt/(cnt-1)))			
			if (!asSample) {
				#st <- sqrt( st^2 * (cnt / (cnt-1)))
				st <- sqrt( st^2 * ((cnt-1) / cnt))						

			}
		} else if (stat == 'mean') {
			st <- st / cnt
			
		} else if (stat == 'rms') {
			if (asSample) {
				st <- sqrt(sumsq/(cnt-1))
			} else {
				st <- sqrt(sumsq/cnt)
			}
			
		} else if (stat == 'skew') {
			if (asSample) {
				stsd <- sqrt(sumsq/(cnt-1))^3
			} else {
				stsd <- sqrt(sumsq/cnt)^3
			}
			st <- d3 / (cnt*stsd)
		}		
		return(st)
	}
)

