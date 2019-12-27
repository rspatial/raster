# Author: Robert J. Hijmans
# Date: June 2010
# Version 1.0
# Licence GPL v3


.readRowsNetCDF <- function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1)) {

	if ( x@file@toptobottom ) { 
		row <- x@nrows - row - nrows + 2 
	}
	is.open <- x@file@open
	if (is.open) {
		nc <- x@file@con
	} else {
		nc <- ncdf4::nc_open(x@file@name, suppress_dimvals = TRUE)
		on.exit( ncdf4::nc_close(nc) )		
	}
	
	zvar <- x@data@zvar

	if (nc$var[[zvar]]$ndims == 1) {
		# for GMT
		ncx <- ncol(x)
		start <- (row-1) * ncx + 1
		count <- nrows * ncx 
		d <- ncdf4::ncvar_get( nc, varid=zvar,  start=start, count=count )		
		if (col > 1 | ncols < ncx) {
			d <- matrix(d, ncol=ncx, byrow=TRUE)
			d <- d[, col:(col+ncols-1)]
			d <- as.vector(t(d))
		}

	
	} else if (nc$var[[zvar]]$ndims == 2) {
		start <- c(col, row)
		count <- c(ncols, nrows)
		d <- ncdf4::ncvar_get(nc, varid=zvar, start=order_count_dim(x, start), count=order_count_dim(x, count), collapse_degen = FALSE )
		d <- aperm(d, perm = x@file@dimreadorder)
	} else if (nc$var[[zvar]]$ndims == 3) {
		start <- c(col, row, x@data@band)
		count <- c(ncols, nrows, 1)
		d <- ncdf4::ncvar_get(nc, varid=zvar, start=order_count_dim(x, start), count=order_count_dim(x, count), collapse_degen = FALSE )
		d <- aperm(d, perm = x@file@dimreadorder)[, , 1]
	} else {
		if (x@data@dim3 == 4) {
			start <- c(col, row, x@data@level, x@data@band)
			count <- c(ncols, nrows, 1, 1)
  		d <- ncdf4::ncvar_get(nc, varid=zvar, start=order_count_dim(x, start), count=order_count_dim(x, count), collapse_degen = FALSE )
  		d <- aperm(d, perm = x@file@dimreadorder)[, , 1, 1]
		} else {
			start <- c(col, row, x@data@band, x@data@level)
			count <- c(ncols, nrows, 1, 1)
  		d <- ncdf4::ncvar_get(nc, varid=zvar, start=order_count_dim(x, start), count=order_count_dim(x, count), collapse_degen = FALSE )
  		d <- aperm(d, perm = x@file@dimreadorder)[, , 1, 1]
		}
	}
	

	#if (!is.na(x@file@nodatavalue)) { d[d==x@file@nodatavalue] <- NA }
	#d <- x@data@add_offset + d * x@data@scale_factor
	
	if (length(dim(d)) > 1) {
		if ( x@file@toptobottom ) { 
			d <- d[, ncol(d):1] 	
		}
	}
	d <- as.vector(d) 
	d[d == x@file@nodatavalue] <- NA
	return(d)	
}
	
	
	
.readRowsBrickNetCDF <- function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1), lyrs) {


# RH removed because of bug with RasterLayer specific slots
#	if (nlayers(x) == 1) {
#		return(.readRowsNetCDF(x=x, row=row, nrows=nrows, col=col, ncols=ncols) )
#	}

	is.open <- x@file@open
	
	if ( x@file@toptobottom ) { 
		row <- x@nrows - row - nrows + 2
	}
	navalue <- x@file@nodatavalue
	
	
	#n the true number of layers
	#nn the span of layers between the first and the last
	#alyrs, the layers requested, scaled to start at one.
	n <- nn <- nlayers(x)
	if (missing(lyrs)) {
		layer <- 1
		lyrs <- 1:n
	} else {
		lyrs <- lyrs[lyrs %in% 1:n]
		if (length(lyrs) == 0) {
			stop("no valid layers")
		}
		layer <- lyrs[1]
		n <- length(lyrs)
		nn <- lyrs[length(lyrs)] - lyrs[1] + 1
	}
	alyrs <- lyrs - lyrs[1] + 1
	lns <- names(x)[lyrs]
	
	nrows <- min(round(nrows), x@nrows-row+1)
	ncols <- min((x@ncols-col+1), ncols)
	stopifnot(nrows > 0)
	stopifnot(ncols > 0)

	if (is.open) {
		nc <- x@file@con
	} else {
		nc <- ncdf4::nc_open(x@file@name, suppress_dimvals = TRUE)
		on.exit( ncdf4::nc_close(nc) )		
	}
	zvar <- x@data@zvar
	
	if (nc$var[[zvar]]$ndims == 4) {
		if (x@data@dim3 == 4) {
			start <- c(col, row, x@data@level, layer)
			count <- c(ncols, nrows, 1, nn)
    	d <- ncdf4::ncvar_get(nc, varid=zvar, start=order_count_dim(x, start), count=order_count_dim(x, count), collapse_degen = FALSE )
    	d <- aperm(d, perm = x@file@dimreadorder)[ , , 1, ]
		} else {
			start <- c(col, row, layer, x@data@level)
			count <- c(ncols, nrows, nn, 1)
    	d <- ncdf4::ncvar_get(nc, varid=zvar, start=order_count_dim(x, start), count=order_count_dim(x, count), collapse_degen = FALSE )
    	d <- aperm(d, perm = x@file@dimreadorder)[ , , , 1]
		}		
	} else {
		start <- c(col, row, layer)
		count <- c(ncols, nrows,  nn)
  	d <- ncdf4::ncvar_get(nc, varid=zvar, start=order_count_dim(x, start), count=order_count_dim(x, count), collapse_degen = FALSE )
  	d <- aperm(d, perm = x@file@dimreadorder)
	}
	

	#if (!is.na(x@file@nodatavalue)) { 	d[d==x@file@nodatavalue] <- NA	}
	#d <- x@data@add_offset + d * x@data@scale_factor
	
	if (nlayers(x) > 1) {
		dims = dim(d)

		if (length(dims) == 3) {
			if ( x@file@toptobottom ) { 
				v <- matrix(nrow=nrows*ncols, ncol=n)
				for (i in 1:length(alyrs)) {
					x <- d[,,alyrs[i]]
					v[,i] <- as.vector( x[, ncol(x):1] )
				}
			} else {
				dim(d) = c(dims[1] * dims[2], dims[3])
				d <- d[, alyrs, drop=FALSE]
				d[d == x@file@nodatavalue] <- NA
				return(d)
			}
		} else if (length(dims) == 2) {
			if (nrows==1) {
				d <- d[ , alyrs,drop=FALSE]
				d[d == navalue] <- NA
				return(d)
				
			} else if (n==1) {
				v <- matrix(nrow=nrows*ncols, ncol=n)
				if ( x@file@toptobottom ) { 
					v[] <- as.vector(d[,ncol(d):1])
				} else {
					v[] <- as.vector(d)				
				}
				
			} else if (ncols==1) {
				if ( x@file@toptobottom ) { 
					d <- d[nrow(d):1, ]
				}
				d <- d[ , alyrs, drop=FALSE]
				d[d == navalue] <- NA
				return(d)
			}
		} else { # length(dims) == 1
			v <- matrix(nrow=nrows*ncols, ncol=n)
			if ( x@file@toptobottom & nrows > 1) {
				d <- rev(d)
			}
			
			v[] <- d # d[, alyrs, drop=FALSE]
		}
	} else {
		if ( x@file@toptobottom ) { 
			if (is.matrix(d)) {
				d <- d[, ncol(d):1]
			}
		} 
		v <- matrix(as.vector(d), ncol=1)
		#v <- v[,lyrs,drop=FALSE]
	}
	
	v[v == navalue] <- NA
	colnames(v) <- lns
	return(v)
}


order_count_dim <- function(x, count){
  if (length(count) == 1) {return(count)}
  dimreadorder <- x@file@dimreadorder
  count[dimreadorder] <- count
  return(count) 
}
