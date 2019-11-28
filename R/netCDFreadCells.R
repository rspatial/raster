# Author: Robert J. Hijmans
# Date: June 2010
# Version 1.0
# Licence GPL v3


.readRasterCellsNetCDF <- function(x, cells) {

	if (canProcessInMemory(x, 2)) {
    # read all
		r <- getValues(x)
		r <- r[cells]
		return(r)
	} 
	

	row1 <- rowFromCell(x, min(cells))
	row2 <- rowFromCell(x, max(cells))
	if ((row2 - row1) < 10 ) {
	# read only rows needed	
		ncl <- (row2 - row1 + 1) * x@ncols
		r <- raster(nrow=1, ncol=ncl)
		v <- getValues(x, row1, row2-row1+1)
		v <- v[cells-cellFromRowCol(x, row1, 1)+1]
		return(v)
	}
	
# read row by row
	colrow <- matrix(ncol=3, nrow=length(cells))
	colrow[,1] <- colFromCell(x, cells)
	colrow[,2] <- rowFromCell(x, cells)
	colrow[,3] <- NA
	rows <- sort(unique(colrow[,2]))
	readrows <- rows
	if ( x@file@toptobottom ) { 
		readrows <- x@nrows - readrows + 1	
	}

	zvar = x@data@zvar
	time = x@data@band
	
	nc <- ncdf4::nc_open(x@file@name, suppress_dimvals = TRUE)
	on.exit( ncdf4::nc_close(nc) )		
	getfun <- ncdf4::ncvar_get

	if (nc$var[[zvar]]$ndims == 1) {
		ncx <- x@ncols
		count <- ncx
		for (i in 1:length(rows)) {
			start <- (readrows[i]-1) * ncx + 1
			v <- as.vector(getfun(nc, varid=zvar, start=start, count=count))
			thisrow <- subset(colrow, colrow[,2] == rows[i])
			colrow[colrow[,2]==rows[i], 3] <- v[thisrow[,1]]
		}	
	} else	if (nc$var[[zvar]]$ndims == 2) {
		count <- c(x@ncols, 1)
		for (i in 1:length(rows)) {
			start <- c(1, readrows[i])
			v <- as.vector(getfun(nc, varid=zvar, start=start, count=count))
			thisrow <- subset(colrow, colrow[,2] == rows[i])
			colrow[colrow[,2]==rows[i], 3] <- v[thisrow[,1]]
		}	
	} else if (nc$var[[zvar]]$ndims == 3) {
		count <- c(x@ncols, 1, 1)
		for (i in 1:length(rows)) {
			start <- c(1, readrows[i], time)
			v <- as.vector(getfun(nc, varid=zvar, start=start, count=count))
			thisrow <- subset(colrow, colrow[,2] == rows[i])
			colrow[colrow[,2]==rows[i], 3] <- v[thisrow[,1]]
		}	
	} else {
		if (x@data@dim3 == 4) {
			count <- c(x@ncols, 1, 1, 1)
			for (i in 1:length(rows)) {
				start <- c(1, readrows[i], x@data@level, time)
				v <- as.vector(getfun(nc, varid=zvar, start=start, count=count))
				thisrow <- subset(colrow, colrow[,2] == rows[i])
				colrow[colrow[,2]==rows[i], 3] <- v[thisrow[,1]]
			}
		} else {
			count <- c(x@ncols, 1, 1, 1)
			for (i in 1:length(rows)) {
				start <- c(1, readrows[i], time, x@data@level)
				v <- as.vector(getfun(nc, varid=zvar, start=start, count=count))
				thisrow <- subset(colrow, colrow[,2] == rows[i])
				colrow[colrow[,2]==rows[i], 3] <- v[thisrow[,1]]
			}
		}
	}
	
	colrow <- colrow[,3]
	#if (!is.na(x@file@nodatavalue)) { colrow[colrow==x@file@nodatavalue] <- NA	}
	#colrow <- x@data@add_offset + colrow * x@data@scale_factor

	colrow[colrow == x@file@nodatavalue] <- NA
	return(colrow) 
}



.readBrickCellsNetCDF <- function(x, cells, layer, nl) {

	i <- which(!is.na(cells))
	
	
	if (length(cells) > 1000) {
		if (canProcessInMemory(x, 2)) {
# read all
			endlayer <- layer+nl-1
			r <- getValues(x)
			r <- r[cells, layer:endlayer]
			return(r)
		}
	} 

	
# read cell by cell
	zvar <- x@data@zvar
	dim3 <- x@data@dim3
	cols <- colFromCell(x, cells)
	rows <- rowFromCell(x, cells)
	if ( x@file@toptobottom ) { 
		rows <- x@nrows - rows + 1 
	}
		

	nc <- ncdf4::nc_open(x@file@name, suppress_dimvals = TRUE)
	on.exit( ncdf4::nc_close(nc) )		
	getfun <- ncdf4::ncvar_get
	
	# this needs to be optimized. Read chunks and extract cells
	j <- which(!is.na(cells))
	if (nc$var[[zvar]]$ndims == 2) {
		count <- c(1, 1)
		res <- matrix(NA, nrow=length(cells), ncol=1)
		for (i in j) {
			start <- c(cols[i], rows[i])
			res[i] <- getfun(nc, varid=zvar, start=start, count=count)
		}	
	} else if (nc$var[[zvar]]$ndims == 3) {
		count <- c(1, 1, nl)
		res <- matrix(NA, nrow=length(cells), ncol=nl)
		for (i in j) {
			start <- c(cols[i], rows[i], layer)
			res[i,] <- getfun(nc, varid=zvar, start=start, count=count)
		}	
	} else {
		if (x@data@dim3 == 4) {
			count <- c(1, 1, 1, nl)
			res <- matrix(NA, nrow=length(cells), ncol=nl)
			for (i in j) {
				start <- c(cols[i], rows[i], x@data@level, layer)
				res[i,] <- getfun(nc, varid=zvar, start=start, count=count)
			}	
		} else {
			count <- c(1, 1, nl, 1)
			res <- matrix(nrow=length(cells), ncol=nl)
			for (i in 1:length(cells)) {
				start <- c(cols[i], rows[i], layer, x@data@level)
				res[i,] <- getfun(nc, varid=zvar, start=start, count=count)
			}	
		}
	}

	#if (!is.na(x@file@nodatavalue)) { res[res==x@file@nodatavalue] <- NA	}
	#res <- x@data@add_offset + res * x@data@scale_factor

	res[res == x@file@nodatavalue] <- NA
	return(res) 
}




