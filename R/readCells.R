# Author: Robert J. Hijmans
# Date : June 2008
# Version 0.9
# Licence GPL v3

#read data on the raster for cell numbers


.readCells <- function(x, cells, layers) {
	
	if (length(cells) < 1) {
#		cat(cells,"\n")
#		utils::flush.console()
		return(NULL)
	}
	
	
	cells <- round(cells)
	
	cells <- cbind(1:length(cells), cells)
	cells <- cells[order(cells[,2]), ,drop=FALSE]
	uniquecells <- sort(stats::na.omit(unique(cells[,2])))
	uniquecells <- uniquecells[(uniquecells > 0) & (uniquecells <= ncell(x))]
	if (length(uniquecells) == 0) {
		return( matrix(NA, nrow=nrow(cells), ncol=length(layers)) )
	}
#  creates problems with large integers
#  perhaps not needed (or causes problems with merge?)
#	uniquecells <- as.integer(uniquecells)
# now using round (above)

	adjust <- TRUE
	if (length(uniquecells) > 0) {
		if ( inMemory(x) ) {
			vals <- getValues(x)[uniquecells]
			adjust <- FALSE
		} else if ( fromDisk(x) ) {
			driver <- x@file@driver
			if (length(uniquecells) > 250 & canProcessInMemory(x, 4)) {
				vals <- getValues(x)
				if (length(layers) > 1) {
					vals <- vals[uniquecells, layers, drop=FALSE]
				} else {
					vals <- vals[uniquecells]				
				}
				adjust <- FALSE
			} else if (driver == 'gdal') {
				vals <- .readCellsGDAL(x, uniquecells, layers)
			} else if ( .isNativeDriver( driver) ) {  # raster, BIL, ..
				vals <- .readCellsRaster(x, uniquecells, layers)
#			} else if ( driver == 'big.matrix') {
#				vals <- .readBigMatrixCells(x, uniquecells) 
			} else if ( driver == 'netcdf') {
				vals <- .readRasterCellsNetCDF(x, uniquecells) 
			} else if ( driver == 'ascii') {
				# can only have one layer
				vals <- .readCellsAscii(x, uniquecells)
			} else {
				stop('I did not expect the code to get here. Please report')
			}
		} else { 
			stop('no data on disk or in memory')
		}	
	} else {
		return(rep(NA, times=length(cells[,1])))
	}
	
	if (is.null(dim(vals))) { 
		vals <- matrix(vals, ncol=length(layers))
		colnames(vals) <- names(x)[layers]
	}
		
	vals <- cbind(uniquecells, vals)
	vals <- merge(x=cells[,2], y=vals, by=1, all=TRUE)
	vals <- as.matrix(cbind(cells[,1], vals[,2:ncol(vals)]))
#	vals <- vals[order(cells[,1]), 2, drop=FALSE]
	vals <- vals[order(vals[,1]), 2:ncol(vals)]

	if (adjust) {
		if (x@data@gain != 1 | x@data@offset != 0) {
			vals <- vals * x@data@gain + x@data@offset
		}
	}

	# if  NAvalue() has been used.....
	if (.naChanged(x)) {
		if (x@file@nodatavalue < 0) {
			vals[vals <= x@file@nodatavalue] <- NA
		} else {
			vals[vals == x@file@nodatavalue] <- NA
		}
	}
	
	return(vals)
}

 

.readBigMatrixCells <- function(x, cells, layers) {
	
	b <- attr(x@file, 'big.matrix')
	
	if (inherits(x, 'RasterLayer')) {
	
		colrow <- matrix(ncol=3, nrow=length(cells))
		colrow[,1] <- colFromCell(x, cells)
		colrow[,2] <- rowFromCell(x, cells)
		colrow[,3] <- NA
		rows <- sort(unique(colrow[,2]))
		nc <- x@ncols
		
		for (i in 1:length(rows)) {
			v <- b[rows[i],  ]
			thisrow <- colrow[colrow[,2] == rows[i], , drop=FALSE]
			colrow[colrow[,2]==rows[i], 3] <- v[thisrow[,1]]
		}
		colrow[, 3]

	} else {
		b[cells, layers]	
	}
}	
 


.readCellsRaster <- function(x, cells, layers=1) {
	nl <- length(layers)
	res <- vector(length=length(cells)*nl)
	res[] <- NA
	
	if (! x@file@toptobottom) {
		rows <- rowFromCell(x, cells)
		cols <- colFromCell(x, cells)
		rows <- nrow(x) - rows + 1
		cells <- cellFromRowCol(x, rows, cols)
	}
	cells <- cells + x@file@offset
	
	if (nbands(x) > 1) {
		if (inherits(x, 'RasterLayer')) {
			if (.bandOrder(x) == 'BIL') {
				cells <- cells + (rowFromCell(x, cells)-1) * x@ncols * (nbands(x)-1) + (bandnr(x)-1) * x@ncols
			} else if (.bandOrder(x) == 'BIP') {
				cells <- (cells - 1) * nbands(x) + bandnr(x)
			} else if (.bandOrder(x) == 'BSQ') {	
				cells <- cells + (bandnr(x)-1) * ncell(x)
			}
		} else {
			if (.bandOrder(x) == 'BIL') {
				cells <- rep(cells + (rowFromCell(x, cells)-1) * x@ncols * (nbands(x)-1) , each=nl) + (layers-1) * x@ncols
			} else if (.bandOrder(x) == 'BIP') {
				cells <- rep((cells - 1) * nbands(x), each=nl) + layers
			} else if (.bandOrder(x) == 'BSQ') {	
				cells <- rep(cells, each=nl) + (layers-1) * ncell(x)
			}
		}
	}
	
	byteord <- x@file@byteorder
	dsize <- dataSize(x@file@datanotation)
	if (.shortDataType(x@file@datanotation) == "FLT") { 
		dtype <- "numeric"
	} else { 
		dtype <- "integer"
	}
	cells <- (cells-1) * dsize
	signed <- dataSigned(x@file@datanotation)
	if (dsize > 2) { signed <- TRUE }
	
	is.open <- x@file@open
	if (!is.open) {
		x <- readStart(x)
	}

	for (i in seq(along.with=cells)) {
		seek(x@file@con, cells[i])
		res[i] <- readBin(x@file@con, what=dtype, n=1, size=dsize, endian=byteord, signed=signed) 
	}
	if (!is.open) {
		x <- readStop(x)
	}
	
	if (x@file@datanotation == 'INT4U') {
		i <- !is.na(res) & res < 0
		res[i] <- 2147483647 - res[i] 
	}
	
	if (dtype == "numeric") {
		res[is.nan(res)] <- NA
		res[res <= x@file@nodatavalue] <- NA
	} else {
		res[res == x@file@nodatavalue] <- NA
	}
	if (nl > 1) {
		res <- t(matrix(res, nrow=nl))
		colnames(res) <- names(x)[layers]
	}
	return(res)
}

