# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3



setMethod ('show' , 'Extent', 
	function(object) {
		cat('class      :' , class(object), '\n')
		cat('xmin       :' , xmin(object), '\n')
		cat('xmax       :' , xmax(object), '\n')
		cat('ymin       :' , ymin(object), '\n')
		cat('ymax       :' , ymax(object), '\n')
	}
)	
	

setMethod ('show' , 'BasicRaster', 
	function(object) {
		cat('class      :' , class(object), '\n')
		cat('dimensions : ', nrow(object), ', ', ncol(object), ', ', ncell(object),'  (nrow, ncol, ncell)\n', sep="" ) 
		cat('resolution : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
		cat('extent     : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat('crs        :' ,.get_projection(object), '\n')
	}
)


	
setMethod ('show' , 'RasterLayer', 
	function(object) {
		cat('class      :' , class(object), '\n')
		if (rotated(object)) {
			cat('rotated    : TRUE\n')
		}
		if (nbands(object) > 1) { 
			cat('band       :' , bandnr(object), ' (of ', nbands(object), ' bands)\n')	
		}	
		cat('dimensions : ', nrow(object), ', ', ncol(object), ', ', ncell(object),'  (nrow, ncol, ncell)\n', sep="" ) 
		cat('resolution : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
		cat('extent     : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat('crs        :' , .get_projection(object)[1], '\n')
		

		if (hasValues(object)) {
			fd <- object@data@fromdisk
			if (fd) {
				cat('source     :', gsub("\\", "/", filename(object), fixed=TRUE), '\n')
			} else {
				cat('source     : memory\n')			
			}
			cat('names      :', names(object), '\n')
			if (object@data@haveminmax) {
				cat('values     : ', minValue(object), ', ',  maxValue(object), '  (min, max)\n', sep="")
			}
		}


		if (is.factor(object)) {
		
			x <- object@data@attributes[[1]]
			nc <- NCOL(x)
			
			# this can actually happen, but x should be a data.frame anyway
			#if (nc == 1) { # this should never happen
			#	x <- data.frame(value=x)
			#}
			
			maxnl <- 12
			if (nc > maxnl) {
				x <- x[, 1:maxnl]
			}
			
	
			#nfact <- sapply(1:ncol(x), function(i) is.numeric(x[,i]))
			if (nrow(x) > 5) {
				cat('attributes :\n') 
				r <- x[c(1, nrow(x)), ,drop=FALSE]
				for (j in 1:ncol(r)) {
					r[is.numeric(r[,j]) & !is.finite(r[,j]), j] <- NA
				}	
				r <- data.frame(x=c('from:','to :'), r)
				a <- colnames(x)

				colnames(r) <- c('    fields :', a)
				colnames(r) <- c('', a)
				rownames(r) <- NULL
				if (nc > maxnl) {
					r <- cbind(r, '...'=rbind('...', '...'))
				}
				print(r, row.names=FALSE)
			} else {
				cat('attributes :\n') 
				print(x, row.names=FALSE)
			}
			
			
		} else {
				
			z <- getZ(object)
			if (length(z) > 0) {
				name <- names(object@z)
				if (is.null(name)) name <- 'z-value'
				name <- paste(sprintf("%-11s", name), ':', sep='')
				cat(name, as.character(z[1]), '\n')
			}

			if (object@file@driver == 'netcdf') {
				z <- attr(object@data, 'zvar')
				if (!is.null(z)) { cat('zvar       :', z, '\n') } 
				z <- attr(object@data, 'level')
				if (!is.null(z)) { 
					if (z>0) { 
						cat('level      :', z, '\n')  
					}
				}
			}
		}
		cat ('\n')
	}
)


setMethod ('show' , 'RasterBrick',
	function ( object ) {
		cat ('class      :' , class ( object ) , '\n')
		if (rotated(object)) {
			cat('rotated    : TRUE\n')
		}
		
		mnr <- 15
		nl <- nlayers(object)
		cat ('dimensions : ', nrow(object), ', ', ncol(object), ', ', ncell(object), ', ', nl, '  (nrow, ncol, ncell, nlayers)\n', sep="" ) 
		#cat ('ncell      :' , ncell(object), '\n')
		cat ('resolution : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
		cat ('extent     : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat ('crs        :' ,.get_projection(object), '\n')

		ln <- names(object)
		if (nl > mnr) {
			ln <- c(ln[1:mnr], '...')
		}

		if (hasValues(object)) {
			fd <- object@data@fromdisk
			if (fd) {
				cat('source     :', gsub("\\", "/", filename(object), fixed=TRUE), '\n')
			} else {
				cat('source     : memory\n')			
			}
			
			if (object@data@haveminmax) {
				minv <- format(minValue(object))
				maxv <- format(maxValue(object))
				minv <- gsub('Inf', '?', minv)
				maxv <- gsub('-Inf', '?', maxv)
				if (nl > mnr) {
					minv <- c(minv[1:mnr], '...')
					maxv <- c(maxv[1:mnr], '...')
				}
				
				
				n <- nchar(ln)
				if (nl > 5) {
					b <- n > 26
					if (any(b)) {
						mid <- floor(n/2)
						ln[b] <- paste(substr(ln[b], 1, 9), '//', substr(ln[b], nchar(ln[b])-9, nchar(ln[b])), sep='')
					}
				}
				
				w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
				m <- rbind(ln, minv, maxv)
				# a loop because 'width' is not recycled by format
				for (i in 1:ncol(m)) {
					m[,i]   <- format(m[,i], width=w[i], justify="right")
				}
				cat('names      :', paste(m[1,], collapse=', '), '\n')
				cat('min values :', paste(m[2,], collapse=', '), '\n')
				cat('max values :', paste(m[3,], collapse=', '), '\n')

			} else {
				cat('names      :', paste(ln, collapse=', '), '\n')
			}			
		} 

		z <- getZ(object)
		if (length(z) > 0) {
			name <- names(object@z)
			if (is.null(name)) name <- 'z-value'
			name <- paste(sprintf("%-11s", name), ':', sep='')
			if (length(z) < mnr) {
				cat(name, paste(as.character(z), collapse=', '), '\n')
			} else {
				cat(name, paste(as.character(range(z)), collapse=', '), '(min, max)\n')
			}
		}
		
		if (object@file@driver == 'netcdf') {
			z <- attr(object@data, 'zvar')
			if (!is.null(z)) { cat('varname    :', z, '\n') } 
			z <- attr(object@data, 'level')
			if (!is.null(z)) { 
				if (z>0) { 
					cat('level      :', z, '\n')  
				}
			}
		}
		
		cat ('\n')
	}
)




setMethod ('show' , 'RasterStack',
	function ( object ) {
		cat ('class      :' , class ( object ) , '\n')
		if (rotated(object)) {
			cat('rotated    : TRUE\n')
		}
		
		mnr <- 15		
		if (filename(object) != '') {
			cat ('filename   :' , filename(object), '\n')
		}
		nl <- nlayers(object)
		if (nl == 0) {
			cat ('nlayers    :' , nl, '\n')
		} else {
			cat ('dimensions : ', nrow(object), ', ', ncol(object), ', ', ncell(object), ', ', nl, '  (nrow, ncol, ncell, nlayers)\n', sep="" ) 
			#cat ('ncell      :' , ncell(object), '\n')
			cat ('resolution : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
			cat ('extent     : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
			cat ('crs        :' ,.get_projection(object), '\n')
			ln <- names(object)
			if (nl > mnr) {
				ln <- c(ln[1:mnr], '...')
			}
			n <- nchar(ln)
			if (nl > 5) {
				b <- n > 26
				if (any(b)) {
					ln[b] <- paste(substr(ln[b], 1, 9), '//', substr(ln[b], nchar(ln[b])-9, nchar(ln[b])), sep='')
				}
			}
			
			minv <- minValue(object)
			if (all(is.na(minv))) {
				cat('names      :', paste(ln, collapse=', '), '\n')
			
			} else {
				minv <- format(minv)
				maxv <- format(maxValue(object))
				minv <- gsub('NA', '?', minv)
				maxv <- gsub('NA', '?', maxv)
				if (nl > mnr) {
					minv <- c(minv[1:mnr], '...')
					maxv <- c(maxv[1:mnr], '...')
				}
				w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
				m <- rbind(ln, minv, maxv)
				# a loop because 'width' is not recycled by format
				for (i in 1:ncol(m)) {
					m[,i]   <- format(m[,i], width=w[i], justify="right")
				}
				cat('names      :', paste(m[1,], collapse=', '), '\n')
				cat('min values :', paste(m[2,], collapse=', '), '\n')
				cat('max values :', paste(m[3,], collapse=', '), '\n')
			}
		}
		
		
		z <- getZ(object)
		if (length(z) > 0) {
			name <- names(object@z)
			if (is.null(name)) name <- 'z-value'
			if (name == '') name <- 'z-value'
			name <- paste(sprintf("%-12s", name), ':', sep='')
			if (length(z) < mnr) {
				cat(name, paste(as.character(z), collapse=', '), '\n')
			} else {
				z <- range(z)
				cat(name, paste(as.character(z), collapse=' - '), '(range)\n')
			}
		}
		
		cat ('\n')
	}
)


setMethod ('show' , '.RasterList', 
	function(object) {
		cat('class      :' , class(object), '\n')
		cat('length     : ', length(object), '\n', sep="" ) 
	}
)
