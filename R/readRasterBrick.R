# Author: Robert J. Hijmans
# Date: Sept 2009
# Version 1.0
# Licence GPL v3


.readRasterBrickValues <- function(object, startrow, nrows=1, startcol=1, ncols=ncol(object)) {

	if (nrows < 1) { stop("nrows should be > 1") }
	startrow <- min(max(1, round(startrow)), object@nrows)
	endrow <- min(object@nrows, startrow+nrows-1)
	nrows <- endrow - startrow + 1

	if (ncols < 1) { stop("ncols should be > 1") }
	startcol <- min(max(1, round(startcol)), object@ncols)
	endcol <- min(object@ncols, startcol+ncols-1)
	ncols <- endcol - startcol + 1
		
	if (.isNativeDriver(object@file@driver))  {

		getBSQData <- function(raster, r, nrows, c, ncols, dtype, dsize, dsign) {
			if (c==1 & ncols==raster@ncols ) {
				if (r==1 & nrows==raster@nrows) {
					nc <- nrows*ncols*raster@data@nlayers
					seek(raster@file@con, raster@file@offset * dsize)
					result <- readBin(raster@file@con, what=dtype, n=nc, dsize, dsign, endian=raster@file@byteorder) 
					dim(result) <- c(nrows*ncols, raster@data@nlayers)
				} else {
					ncells <- nrows*ncols
					result <- matrix(nrow=ncells, ncol=raster@data@nlayers)
					for (b in 1:raster@data@nlayers) {
						offset <- raster@file@offset + (b-1) * raster@ncols * raster@nrows + (r-1) * raster@ncols 
						seek(raster@file@con, offset * dsize)
						result[,b] <- readBin(raster@file@con, what=dtype, n=ncells, dsize, dsign, endian=raster@file@byteorder) 
					}
				}
			} else {
				nc <- nrows*ncols
				result <- matrix(nrow=nc, ncol=raster@data@nlayers)
				res <- matrix(ncol=nrows, nrow=ncols)
				for (b in 1:raster@data@nlayers) {
					offset <- raster@file@offset + (b-1) * raster@ncols * raster@nrows + (r-1) * raster@ncols + (c-1)
					for (i in 1:nrows) {
						off <- offset + (i-1) * raster@ncols 
						seek(raster@file@con, off * dsize)
						res[,i] <- readBin(raster@file@con, what=dtype, n=ncols, dsize, dsign, endian=raster@file@byteorder) 
					}
					result[,b] <- as.vector(res)
				}
			}
			return( result )
		}
		
		getBilData <- function(raster, r, nrows, c, ncols, dtype, dsize, dsign) {
			if (c==1 & ncols==raster@ncols ) {
				nc <- nrows*ncols*raster@data@nlayers
				if (r==1 & nrows==raster@nrows) {
					seek(raster@file@con, raster@file@offset * dsize)
					res <- readBin(raster@file@con, what=dtype, n=nc, dsize, dsign, endian=raster@file@byteorder) 
				} else {
					offset <- raster@file@offset + raster@data@nlayers * raster@ncols * (r-1) 
					seek(raster@file@con, offset * dsize)
					res <- readBin(raster@file@con, what=dtype, n=nc, dsize, dsign, endian=raster@file@byteorder) 
				}
			} else {
				res <- matrix(ncol=nrows*raster@data@nlayers, nrow=ncols)
				offset <- raster@file@offset + raster@data@nlayers * raster@ncols * (r-1) + (c-1)
				for (i in 1:ncol(res)) {
						off <- offset + (i-1) * raster@ncols
						seek(raster@file@con, off * dsize)
						res[,i] <- readBin(raster@file@con, what=dtype, n=ncols, dsize, dsign, endian=raster@file@byteorder) 
				}
				res <- as.vector(res)
			}
			
			result <- matrix(nrow=ncols*nrows, ncol=nlayers(raster))
			dim(res) <- c(ncols, raster@data@nlayers*nrows)
			a <- rep(1:raster@data@nlayers, nrows)
			for (b in 1:raster@data@nlayers) {
				result[,b] <- as.vector(res[,a==b])
			}
			return(result)
		}

		getBipData <- function(raster, r, nrows, c, ncols, dtype, dsize, dsign) {
			if (c==1 & ncols==raster@ncols ) {
				nc <- nrows*ncols*raster@data@nlayers
				if (r==1 & nrows==raster@nrows) {
					seek(raster@file@con, raster@file@offset * dsize)
					result <- readBin(raster@file@con, what=dtype, n=nc, dsize, dsign, endian=raster@file@byteorder) 
				} else {
					offset <- raster@file@offset + raster@data@nlayers * raster@ncols * (r-1) 
					seek(raster@file@con, offset * dsize)
					result <- readBin(raster@file@con, what=dtype, n=nc, dsize, dsign, endian=raster@file@byteorder) 
				}
			} else {
				nc <- ncols*raster@data@nlayers
				result <- matrix(ncol=nrows, nrow=ncols*raster@data@nlayers)
				offset <- raster@file@offset + raster@data@nlayers * raster@ncols * (r-1) 
				for (i in 1:nrows) {
					off <- offset + (i-1) * raster@data@nlayers * raster@ncols + (c-1) * raster@data@nlayers
					seek(raster@file@con, off * dsize)
					result[,i] <- readBin(raster@file@con, what=dtype, n=nc, dsize, dsign, endian=raster@file@byteorder) 
				}
				result <- as.vector(result)
			}
			dim(result) <- c(raster@data@nlayers, nrows*ncols)
			t(result)
		}

		
		if (! object@file@toptobottom ) {
			stop('bottom-to-top data not supported for RasterBrick objects')
		}
		dtype <- substr(object@file@datanotation, 1, 3)
		if (dtype == "INT" | dtype == "LOG" ) { 
			dtype <- "integer"
		} else {
			dtype <- "numeric" 
		}
		dsize <- dataSize(object@file@datanotation)
		dsign <- dataSigned(object@file@datanotation)
		if (dsize > 2) { dsign <- TRUE }
		
		is.open <- object@file@open
		if (!is.open) {
			object <- readStart(object)
		}
		if (object@data@nlayers > 1) {
			bo <- object@file@bandorder
			if (bo == 'BSQ') {
				result <- getBSQData(object, r=startrow, nrows=nrows, c=startcol, ncols=ncols, dtype=dtype, dsize=dsize, dsign=dsign) 
			} else if (bo == 'BIL') {
				result <- getBilData(object, r=startrow, nrows=nrows, c=startcol, ncols=ncols, dtype=dtype, dsize=dsize, dsign=dsign) 
			} else if (bo == 'BIP') {
				result <- getBipData(object, r=startrow, nrows=nrows, c=startcol, ncols=ncols, dtype=dtype, dsize=dsize, dsign=dsign) 
			} 
		} else {
			result <- getBSQData(object, r=startrow, nrows=nrows, c=startcol, ncols=ncols, dtype=dtype, dsize=dsize, dsign=dsign) 
		}
		if (!is.open) {
			object <- readStop(object)
		}
			

#		result[is.nan(result)] <- NA

		if (object@file@datanotation == 'INT4U') {
			i <- !is.na(result) & result < 0
			result[i] <- 2147483647 - result[i]
		}

		if (dtype == 'numeric') {
			result[result <= (0.999999 * object@file@nodatavalue)] <- NA 	
			result[is.nan(result)] <- NA
		} else {
			result[result == object@file@nodatavalue ] <- NA 			
		}
		if (dtype == 'logical') {
			result <- as.logical(result)
		}
		
 
 	} else if (object@file@driver == 'netcdf') {
		result <- .readRowsBrickNetCDF(object, startrow, nrows, startcol, ncols)
	
 	} else if (object@file@driver == 'big.matrix') {

		b <- attr(object@file, 'big.matrix')
		start <- cellFromRowCol(object, startrow, startcol)
		end <- cellFromRowCol(object, endrow, endcol)
		result <- b[start:end, ]
 
	} else {
	#use GDAL  			
        offs <- c((startrow - 1), (startcol - 1))
        reg <- c(nrows, ncols)
        con <- rgdal::GDAL.open(object@file@name, silent = TRUE)

#		result <- rgdal::getRasterData(con, offset=offs, region.dim=reg)
#		result <- do.call(cbind, lapply(1:nlayers(object), function(i) as.vector(result[,,i])))
# just as fast, it seems:
        result <- matrix(nrow = ncols * nrows, ncol = nlayers(object))
        for (b in 1:object@data@nlayers) {
            result[, b] <- rgdal::getRasterData(con, offset = offs, 
                region.dim = reg, band = b)
        }

        rgdal::closeDataset(con)
        result[result == object@file@nodatavalue] <- NA
		
	}

	if (object@data@gain != 1 | object@data@offset != 0) {
		result <- result * object@data@gain + object@data@offset
	}

	colnames(result) <- names(object)
	return(result)
}



