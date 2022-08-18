# Author: Robert J. Hijmans
# Date : December 2010
# Version 0.9
# Licence GPL v3
	
if (!isGeneric("update")) {
	setGeneric("update", function(object, ...)
		standardGeneric("update"))
}	

setMethod('update', signature(object='RasterLayer'), 
function(object, v, cell, ...) {

	if (!fromDisk(object)) { 
		stop('object is not associated with a file on disk.')
	}

	band <- bandnr(object)
	cell <- stats::na.omit(round(cell))

	driver <- object@file@driver
	if (.isNativeDriver(driver)) {
		stopifnot(object@file@toptobottom)

		if (nbands(object) > 1) {
			b <- brick(filename(object), native=TRUE)
			b <- update(b, v, cell, band=bandnr(object))
			r <- raster(filename(object), band=bandnr(object))
			return(r)
		}
	}
	
	datatype <- object@file@datanotation
	dtype <- substr(datatype, 1, 3)
	v <- .checkData(object, v, cell, dtype)
	
	setminmax <- FALSE
	if (object@data@haveminmax) {
		lst <- .updateMinMax(object, v, cell, 1) # band=1 because there is only one set of min/max values
		object <- lst[[1]]
		setminmax <- lst[[2]]
	}

	if (driver == 'gdal') {	
		return( .updateGDAL(object, v, cell, band, setminmax) )
	} else if (driver == 'netcdf') {	
		return( .updateNCDF(object, v, cell, band ) )
	} else if (.isNativeDriver(driver)) {
		return( .updateNativeSingle(object, v, cell, band, driver, datatype ) )
	}	
	
	stop('not implemented for:  ', driver, '  files')
}	
)



setMethod('update', signature(object='RasterBrick'), 
function(object, v, cell, band, ...) {

	if (!fromDisk(object)) { 
		stop('object is not associated with a file on disk.')
	}

	stopifnot(band > 0 & band <= nbands(object))

	cell <- stats::na.omit(round(cell))

	datatype <- object@file@datanotation
	dtype <- substr(datatype, 1, 3)
	v <- .checkData(object, v, cell, dtype)
	
	setminmax <- FALSE
	if (object@data@haveminmax) {
		setminmax <- FALSE
		if (object@data@haveminmax) {
			object <- .updateMinMax(object, v, cell, band)
			setminmax <- object[[2]]
			object <- object[[1]]
		}
	}

	driver <- object@file@driver

	if (driver == 'gdal') {	
		return( .updateGDAL(object, v, cell, band, setminmax) )
	} else if (driver == 'netcdf') {
		return( .updateNCDF(object, v, cell, band ) )
	} else if (.isNativeDriver(driver)) {
		stopifnot(object@file@toptobottom)
		return ( .updateNativeMultiple(object, v, cell, band, driver, datatype ) )
	}

	stop('not implemented for:  ', driver, '  files')
}
)


.updateNativeSingle <- function(object, v, cell, band, driver, datatype) {
		
		minv <- object@data@min
		maxv <- object@data@max
			
		object <- writeStart(object, filename(object), update=TRUE, format=driver, datatype=datatype, overwrite=TRUE)

		dtype <- substr(datatype, 1, 3)
		if (dtype == "INT" | dtype == "LOG") { 
			v[is.na(v)] <- as.integer(object@file@nodatavalue)		
		} else { 
			v[] <- as.numeric(v) 
		}

		if (is.matrix(v)) {
			for (r in 1:nrow(v)) {
				pos <- (cell-1) * object@file@dsize
				seek(object@file@con, pos, rw='w')
				writeBin(v[r,], object@file@con, size=object@file@dsize )
				cell <- cell + object@ncols
			}
		
		} else {
			if (length(cell) == 1) {
				pos <- (cell-1) * object@file@dsize
				seek(object@file@con, pos, rw='w')
				writeBin(v, object@file@con, size=object@file@dsize )
			} else {
				for (i in 1:length(cell)) {
					pos <- (cell[i]-1) * object@file@dsize
					seek(object@file@con, pos, rw='w')
					writeBin(v[i], object@file@con, size=object@file@dsize )
				}
			}
		}
		
		object@data@min <- minv
		object@data@max <- maxv
		object@data@haveminmax <- TRUE
		object <- writeStop(object) 
		if (object@data@min == Inf) {
			object@data@haveminmax <- FALSE
			if (ncell(object) <= 1000000) {
				object <- setMinMax(object)
				hdr(object, driver)
			}
		}
		return( object )
}




.updateNativeMultiple <- function(object, v, cell, band, driver, datatype ) {
	# need to support this too:
		stopifnot(object@file@toptobottom)

		bandorder <- object@file@bandorder

		getoff <- function(object, cell) {
			if (bandorder == 'BIL') {
				rc <- rowColFromCell(object, cell) - 1
				off <- ((nbands(object) * (rc[1]) + (band-1)) * object@ncols + rc[2] ) * object@file@dsize
			} else if (bandorder == 'BIP') {
				off <- (nbands(object) * (cell-1) + band-1) * object@file@dsize
			} else if (bandorder == 'BSQ') {
				off <- (ncell(object) * (band-1) + (cell-1)) * object@file@dsize
			} else {
				stop("unknown band order")
			}
			return(off)
		}
		
		minv <- object@data@min
		maxv <- object@data@max
			
		object <- writeStart(object, filename(object), update=TRUE, format=driver, datatype=datatype, overwrite=TRUE, bandorder=bandorder)
		
		dtype <- substr(datatype, 1, 3)
		if (dtype == "INT" | dtype == "LOG") { 
			v[is.na(v)] <- as.integer(object@file@nodatavalue)		
		} else { 
			v[] <- as.numeric(v) 
		}

		if (is.matrix(v)) {
			if (bandorder == 'BIP') {
				for (r in 1:nrow(v)) {
					for (c in 1:ncol(v)) {
						pos <- getoff(object, cell+c-1)
						seek(object@file@con, pos, rw='w')
						writeBin(v[r,c], object@file@con, size=object@file@dsize )
					}
					cell <- cell + object@ncols
				}
			} else {
				for (r in 1:nrow(v)) {
					pos <- getoff(object, cell)
					seek(object@file@con, pos, rw='w')
					writeBin(v[r,], object@file@con, size=object@file@dsize )
					cell <- cell + object@ncols
				}
			} 
		
		} else {
			if (length(cell) == 1) {
				if (bandorder == 'BSQ') {
					pos <- getoff(object, cell)
					seek(object@file@con, pos, rw='w')
					writeBin(v, object@file@con, size=object@file@dsize )
				} else if (bandorder == 'BIP') {
					for (i in 1:length(v)) {
						pos <- getoff(object, cell+i-1)
						seek(object@file@con, pos, rw='w')
						writeBin(v[i], object@file@con, size=object@file@dsize )
					}	
				} else {
					cell2 <- cell+length(v)-1
					rows <- rowFromCell(object, cell) : rowFromCell(object, cell2) 
					cols <- colFromCell(object, cell) : colFromCell(object, cell2)
					rows <- unique(rows)
					cols <- unique(cols)
					nr <- length(rows)
					if (nr == 1) {
						pos <- getoff(object, cell)			
						seek(object@file@con, pos, rw='w')
						writeBin(v, object@file@con, size=object@file@dsize )
					} else {
						pos <- getoff(object, cellFromRowCol(object, rows[1], cols[1]))			
						seek(object@file@con, pos, rw='w')
						nc <- object@ncols - cols[1]
						writeBin(v[1:nc], object@file@con, size=object@file@dsize )
						v <- v[-(1:nc)]
						if (nr > 2) {
							nc <- object@ncols
							for (i in 3:(nr-1)) {
								pos <- getoff(object, cellFromRowCol(object, rows[i], 1))
								seek(object@file@con, pos, rw='w')
								writeBin(v[1:nc], object@file@con, size=object@file@dsize )
								v <- v[-(1:nc)]
							}
							if (length(v) > 0) {
								pos <- getoff(object, cellFromRowCol(object, rows[nr], 1))
								seek(object@file@con, pos, rw='w')
								writeBin(v, object@file@con, size=object@file@dsize )
							}
						}
					}
				}
			} else {
				for (i in 1:length(cell)) {
					pos <- getoff(object, cell[i])
					seek(object@file@con, pos, rw='w')
					writeBin(v[i], object@file@con, size=object@file@dsize )
				}
			}
		}
		
		object@data@min <- minv
		object@data@max <- maxv
		object@data@haveminmax <- TRUE
		object <- writeStop(object) 
		if (object@data@min[band] == Inf) {
			object@data@haveminmax <- FALSE
			if (ncell(object) * nbands(object) <= 1000000) {
				object <- setMinMax(object)
				hdr(object, driver)
			}
		}
		return( object )
}


.updateNCDF <- function(object, v, cell, band) {
		
		nc <- ncdf4::nc_open(object@file@name, write=TRUE)
		on.exit( ncdf4::nc_close(nc) )		

		zvar <- object@data@zvar
		dims <- nc$var[[zvar]]$ndims
		
		if (dims > 3) {
			# there is code for one level higher, but I am not sure if it is OK, as it does not check the order or the vars.
			stop('not yet implemented for high dimensional (>4) ncdf files')
		}
		if (is.matrix(v)) {
			startrow <- rowFromCell(object, cell)
			startcol <- colFromCell(object, cell)
			if (nc$var[[zvar]]$ndims == 2) {
				try ( ncdf4::ncvar_put(nc, zvar, v, start=c(startcol, startrow), count=c(ncol(v), nrow(v))) )
			} else if (nc$var[[zvar]]$ndims == 3) {
				try ( ncdf4::ncvar_put(nc, zvar, v, start=c(startcol, startrow, band), count=c(ncol(v), nrow(v), 1)) )
			} else if (nc$var[[zvar]]$ndims == 4) {
				try ( ncdf4::ncvar_put(nc, zvar, v, start=c(startcol, startrow, object@data@level, band), count=c(ncol(v), nrow(v), 1, 1)) )
			}
			
		} else {
			if (length(cell) == 1) {
				cell <- cell:(cell+length(v)-1)
				rows <- rowFromCell(object, cell)
				cols <- colFromCell(object, cell)
				rows <- unique(rows)
				cols <- unique(cols)
				nr <- length(rows)
				if (nr == 1) {
					#v <- as.matrix(v)
					if (nc$var[[zvar]]$ndims == 2) {
						try ( ncdf4::ncvar_put(nc, zvar, v, start=c(cols[1], rows), count=c(length(cols), 1)) )
					} else if (nc$var[[zvar]]$ndims == 3) {
						try ( ncdf4::ncvar_put(nc, zvar, v, start=c(cols[1], rows, band), count=c(length(cols), 1, 1)) )
					} else if (nc$var[[zvar]]$ndims == 4) {
						try ( ncdf4::ncvar_put(nc, zvar, v, start=c(cols[1], rows, object@data@level, band), count=c(length(cols), 1, 1, 1)) )
					}
				} else {	
					offset <- c(cols[1], rows[1])
					ncols <- object@ncols - cols[1]
					vv <- v[1:ncols]
					if (nc$var[[zvar]]$ndims == 2) {
						try ( ncdf4::ncvar_put(nc, zvar, vv, start=c(cols[1], rows), count=c(length(cols), 1)) )
					} else if (nc$var[[zvar]]$ndims == 3) {
						try ( ncdf4::ncvar_put(nc, zvar, vv, start=c(cols[1], rows, band), count=c(length(cols), 1, 1)) )
					} else if (nc$var[[zvar]]$ndims == 4) {
						try ( ncdf4::ncvar_put(nc, zvar, vv, start=c(cols[1], rows, object@data@level, band), count=c(length(cols), 1, 1, 1)) )
					}
					v <- v[-(1:nc)]
					if (nr > 2) {
						vv <- v[1:n]
						nrows <- nr-2
						n <- nrows * object@ncols
						if (nc$var[[zvar]]$ndims == 2) {
							try ( ncdf4::ncvar_put(nc, zvar, vv, start=c(1, rows), count=c(ncols, 1)) )
						} else if (nc$var[[zvar]]$ndims == 3) {
							try ( ncdf4::ncvar_put(nc, zvar, vv, start=c(1, rows, band), count=c(ncols, 1, 1)) )
						} else if (nc$var[[zvar]]$ndims == 4) {
							try ( ncdf4::ncvar_put(nc, zvar, vv, start=c(1, rows, object@data@level, band), count=c(ncols, 1, 1, 1)) )
						}
						v <- v[-(1:n)]
					}
					if (nc$var[[zvar]]$ndims == 2) {
						try ( ncdf4::ncvar_put(nc, zvar, vv, start=c(1, rows), count=c(1, rows[nr])) )
					} else if (nc$var[[zvar]]$ndims == 3) {
						try ( ncdf4::ncvar_put(nc, zvar, vv, start=c(1, rows, band), count=c(1, rows[nr], 1)) )
					} else if (nc$var[[zvar]]$ndims == 4) {
						try ( ncdf4::ncvar_put(nc, zvar, vv, start=c(1, rows, object@data@level, band), count=c(1, rows[nr], 1, 1)) )
					}
				} 
			} else {
				rows <- rowFromCell(object, cell)
				cols <- colFromCell(object, cell)
				if (nc$var[[zvar]]$ndims == 2) {
					for (i in 1:length(cell)) {
						try ( ncdf4::ncvar_put(nc, zvar, v[i], start=c(cols[i], rows[i]), count=c(1, 1)) )
					}
				} else if (nc$var[[zvar]]$ndims == 3) {
					for (i in 1:length(cell)) {
						try ( ncdf4::ncvar_put(nc, zvar, v[i], start=c(cols[i], rows[i], band), count=c(1, 1, 1)) )
					}
				} else if (nc$var[[zvar]]$ndims == 4) {
					for (i in 1:length(cell)) {
						try ( ncdf4::ncvar_put(nc, zvar, v[i], start=c(cols[i], rows[i], object@data@level, band), count=c(1, 1, 1, 1)) )
					}
				}
			}
		}
		return( object )
}


.updateGDAL <- function(object, v, cell, band, setminmax) {
	stop("no longer supported")
}

# .updateGDAL <- function(object, v, cell, band, setminmax) {
	# gdal <- methods::new("GDALDataset", filename(object))
	# on.exit( rgdal::GDAL.close(gdal) )

	# dr <- rgdal::getDriverName(rgdal::getDriver(gdal))
	# if (! dr %in% .gdalWriteFormats()[,1]) {
		# stop('cannot update this file format (GDAL driver)')
	# }
		
	# if (is.matrix(v)) {

		# startrow <- rowFromCell(object, cell) - 1
		# startcol <- colFromCell(object, cell) - 1
		# rgdal::putRasterData(gdal, t(v), band=band, offset= c(startrow, startcol) )

	# } else {
		# if (length(cell) == 1) {
			# cell <- cell:(cell+length(v)-1)
			# rows <- rowFromCell(object, cell) - 1
			# cols <- colFromCell(object, cell) - 1
			# rows <- unique(rows)
			# cols <- unique(cols)
			# nr <- length(rows)
			# if (nr == 1) {
				# rgdal::putRasterData(gdal, v, band=band, offset=c(rows, cols[1]))
			# } else {
				# offset <- c(rows[1], cols[1])
				# nc <- object@ncols - cols[1]
				# rgdal::putRasterData(gdal, v[1:nc], band=band, offset=offset)
				# v <- v[-(1:nc)]
				# if (nr > 2) {
					# nrows <- nr-2
					# n <- nrows * object@ncols
					# rgdal::putRasterData(gdal, t(matrix(v[1:n], ncol=object@ncols, byrow=TRUE)), band=band, offset=c(rows[2], 0))
					# v <- v[-(1:n)]
				# }
				# if (length(v) > 0) {
					# rgdal::putRasterData(gdal, v, band=band, offset=c(rows[nr], 0))
				# }
			# } 
		# } else {
			# rows <- rowFromCell(object, cell) - 1
			# cols <- colFromCell(object, cell) - 1
			# for (i in 1:length(cell)) {
				# rgdal::putRasterData(gdal, v[i], band=band, offset=c(rows[i], cols[i]))
			# } 
		# }
			
	# }

	# if (setminmax) {	
		# b <- methods::new("GDALRasterBand", gdal, band)
		# statistics <- c(object@data@min, object@data@max, NA, NA)
		# rgdal::GDALcall(b, "SetStatistics", statistics)	
	# }

	# return(object)
# }


.checkData <- function(object, v, cell, dtype) {

	stopifnot(length(cell) > 0)

	if (is.matrix(v)) {
		if (length(cell) > 1) {
			warning('only first cell used')
			cell <- cell[1] 
		}
		stopifnot(cell > 0)
		
		rc <- rowColFromCell(object, cell)
		if ((nrow(v) + rc[1] - 1) > nrow(object)) { 
			stop('attempting to update beyond end of file') 
		}
		if ((ncol(v) + rc[2] - 1) > ncol(object)) { 
			stop('attempting to update beyond end of file') 
		}
		dm <- dim(v)
		mat <- TRUE
		
	} else {
		stopifnot( is.vector(v) ) 
		if (length(cell) > 1) {
			stopifnot(max(cell) <= ncell(object))
			stopifnot(min(cell) > 0)
			
			if (length(cell) != length(v)) {
				# recycling
				vv <- cell
				vv[] <- v
				v <- vv
			}

			
		} else {
			stopifnot(cell > 0)
			if ((length(v) + cell - 1) > ncell(object)) {
				stop('attempting to update beyond end of file') 
			}
		}
		mat <- FALSE
	}

	if (dtype == "INT" ) { 
		v <- as.integer(round(v)) 
	} else if ( dtype =='LOG' ) {
		v[v != 1] <- 0
		v <- as.integer(v)  
	}
	v[is.infinite(v)] <- NA
	if (mat) {
		dim(v) <- dm
	}
	return(v)
}


.updateMinMax <- function(object, v, cell, band) {
	setminmax <- FALSE
	v <- stats::na.omit(v) 
	newmin <- FALSE
	newmax <- FALSE
	if (length(v) > 0) {
		minv <- min(v)
		maxv <- max(v)
		if (minv < object@data@min[band]) { 
			newmin <- TRUE
		}
		if (maxv > object@data@max[band]) { 
			newmax <- TRUE
		}
	}
	if (newmin & newmax) {
		object@data@min[band] <- minv 
		object@data@max[band] <- maxv
		setminmax <- TRUE
	} else {
		if (is.matrix(v)) {
			rc <- rowColFromCell(object, cell)
			oldv <- getValuesBlock(object, rc[1], nrow(v), rc[2], ncol(v))
		} else {
			if (length(cell) == 1) {
				oldv <- stats::na.omit(.cellValues(object, cell:(cell+length(v)-1)))
			} else {
				oldv <- stats::na.omit(.cellValues(object, cell))
			}
		}
		if (length(oldv) > 0) {
			oldmin <- min(oldv)
			oldmax <- max(oldv)
			if (oldmin > object@data@min[band]) {
				lostmin <- FALSE
			} else {
				lostmin <- TRUE
			}
			if (oldmax < object@data@max[band]) {
				lostmax <- FALSE
			} else {
				lostmax <- TRUE
			}
		} else {
			lostmin <- FALSE
			lostmax <- FALSE
		}
		
		if (! (lostmin | lostmax) ) {
			if (newmin | newmax) {
				object@data@min <- min(object@data@min[band], minv)
				object@data@max <- max(object@data@max[band], maxv)
				setminmax <- TRUE
			}
		} else if ((lostmin & newmin) & (! lostmax)) {
			object@data@min <- min(object@data@min[band], minv)
			setminmax <- TRUE
		} else if ((lostmax & newmax) & (! lostmin)) {
			object@data@max <- max(object@data@max[band], maxv)
			setminmax <- TRUE
		} else {
			object@data@min[band] <- Inf
			object@data@max[band] <- -Inf
			object@data@haveminmax <- FALSE				
			setminmax <- TRUE
		}
	}
	return(list(object, setminmax))
}





# .updateGDALminmax <- function(object, minv, maxv) {
	# gdal <- methods::new("GDALDataset", filename(object))
	# on.exit( rgdal::GDAL.close(gdal) )

	# for (band in 1:nlayers(object)) {
		# b <- methods::new("GDALRasterBand", gdal, band)
		# statistics <- c(minv[band], maxv[band], NA, NA)
		# rgdal::GDALcall(b, "SetStatistics", statistics)	
	# }
	# return(object)
# }
