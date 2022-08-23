# Author: Robert J. Hijmans
# Date :  September 2009
# Version 1.0
# Licence GPL v3



setMethod("writeValues", signature(x="RasterLayer", v="vector"), 
	function(x, v, start, ...) {

		v[is.infinite(v)] <- NA

		datanotation <- x@file@datanotation
		if (substr(datanotation,1,1) != "F") {
			v <- round(v)
			size <- substr(datanotation,4,4)
			if (substr(datanotation, 1, 3) == "LOG") {
				v[v != 1] <- 0
			} else if (substr(datanotation, 5, 5) == "U") {
				v[v < 0] <- NA
				if (size == "1") {
					v[v > 255] <- NA
				} else if (size == "2") {
					v[v > 65535] <- NA
				} else {
					v[v > 4294967295] <- NA				
				}
			} else {
				if (size == "1") {
					v[v < -128] <- NA
					v[v > 127] <- NA
				} else if (size == "2") {
					v[v < -32768] <- NA
					v[v > 32767] <- NA
				} else {
					v[v < -2147483648] <- NA
					v[v > 2147483647] <- NA
				}
			}
		}
		
		rsd <- stats::na.omit(v) # min and max values
		if (length(rsd) > 0) {
			x@data@min <- min(x@data@min, rsd)
			x@data@max <- max(x@data@max, rsd)
		}	

		driver <- x@file@driver
		
		if ( driver == "gdal" ) {
			r <- attr(x@file, "transient")
			writeValues(r, v, start, length(v) / ncol(x))
#			off <- c(start-1, 0)
#			v[is.na(v)] <- x@file@nodatavalue
#			v <- matrix(v, nrow=x@ncols)
#			gd <- rgdal::putRasterData(x@file@transient, v, band=1, offset=off) 	

		} else if ( driver %in% .nativeDrivers() ) {
			if (x@file@dtype == "FLT" ) { 
				# v may be integers, while the filetype is FLT
				v  <- as.numeric( v ) 
				if (driver != "raster") {
					v[is.na(v)] <- x@file@nodatavalue
				}
			
			} else {
				v[is.na(v)] <- as.integer(x@file@nodatavalue)		
				v <- as.integer(v)  
			}
			
			start <- (start-1) * x@ncols * x@file@dsize
			seek(x@file@con, start, rw="w")	
#			print(v)
			writeBin(v, x@file@con, size=x@file@dsize )
			
		} else if ( driver == "netcdf") {

			x <- .writeValuesCDF(x, v, start)
			
#		} else if ( driver == "big.matrix") {
#
#			b <- attr(x@file, "big.matrix")
#			nrows <- length(v) / ncol(x)
#			# b[rowColFromCell(x, start:(start+length(v)-1))] <- v
#			b[start:(start+nrows-1), ] <-  matrix(v, nrow=nrows, byrow=TRUE)

		} else if ( driver == "ascii") {
		
			opsci = options("scipen")
			if (x@file@dtype == "INT") {
				options(scipen=10)
				v <- round(v)				
			} 
			
			v[is.na(v)] <- x@file@nodatavalue
			
			if (x@file@dtype == "FLT") {
				# hack to make sure that ArcGIS does not 
				# assume values are integers if the first 
				# values have no decimal point
				v <- as.character(v)
				v[1] <- formatC(as.numeric(v[1]), 15, format="f")
			}
					
			v <- matrix(v, ncol=ncol(x), byrow=TRUE)
			utils::write.table(v, x@file@name, append = TRUE, quote = FALSE, sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
			options(scipen=opsci)
			
		} else {
			stop("was writeStart used?")
		}
		return(x)
	} 		
)



setMethod("writeValues", signature(x="RasterBrick", v="matrix"), 
	function(x, v, start, ...) {
	
		v[is.infinite(v)] <- NA
		if (is.logical(v)) {
			v[] <- as.integer(v)
		}

		w <- getOption("warn")
		options("warn"=-1) 
		rng <- apply(v, 2, range, na.rm=TRUE)
		x@data@min <- pmin(x@data@min, rng[1,])
		x@data@max <- pmax(x@data@max, rng[2,])
		options("warn"= w) 		
		
		driver <- x@file@driver
		if ( driver %in% .nativeDrivers() ) {
			
			#if (!is.matrix(v)) v <- matrix(v, ncol=1)
			
			if (x@file@dtype == "INT") { 
				v[is.na(v)] <- x@file@nodatavalue		
				dm <- dim(v)
				v <- as.integer(round(v))  
				dim(v) <- dm
			} else if ( x@file@dtype =="LOG" ) {
				v[v != 1] <- 0
				v[is.na(v)] <- x@file@nodatavalue
				dm <- dim(v)
				v <- as.integer(round(v))  
				dim(v) <- dm
			} else { # if (!is.numeric(v)) { 
				v[] <- as.numeric( v ) 
			}

		
			if (x@file@bandorder=="BIL") {
			
				start <- (start-1) * x@ncols * x@file@dsize * nlayers(x)
				seek(x@file@con, start, rw="w")			
				
				loop <- nrow(v) / x@ncols
				start <- 1
				for (i in 1:loop) {
					end <- start + x@ncols - 1
					writeBin(as.vector(v[start:end,]), x@file@con, size=x@file@dsize )
					start <- end + 1
				}
				
			} else if (x@file@bandorder=="BIP") {
			
				start <- (start-1) * x@ncols * x@file@dsize * nlayers(x)
				seek(x@file@con, start, rw="w")	
				writeBin(as.vector(t(v)), x@file@con, size=x@file@dsize )
				
			} else if (x@file@bandorder=="BSQ") {
			
				start <- (start-1) * x@ncols * x@file@dsize
				nc <- ncell(x) * x@file@dsize
				for (i in 1:ncol(v)) {
					pos <- start + nc * (i-1)
					seek(x@file@con, pos, rw="w")
					writeBin(v[,i], x@file@con, size=x@file@dsize )
				}
			} else {
				stop("unknown band order")
			}
			
		} else if ( driver == "netcdf") {

			x <- .writeValuesBrickCDF(x, v, start)

		} else if ( driver == "big.matrix") {

			b <- attr(x@file, "big.matrix")
			startcell <- cellFromRowCol(x, start, 1)
			endcell <- startcell+nrow(v)-1
			b[startcell:endcell, ] <- v
			
		} else { # rgdal
		
			#off <- c(start-1, 0)
			#if (x@file@datanotation == "INT1U") {
			#	v[v < 0] <- NA
			#}

			#v[is.na(v)] <- x@file@nodatavalue
			#for (i in 1:nlayers(x)) {
			#	vv <- matrix(v[,i], nrow=ncol(x))
			#	gd <- rgdal::putRasterData(x@file@transient, vv, band=i, offset=off) 	
			#}
			
			r <- attr(x@file, "transient")	
			writeValues(r, as.vector(t(v)), start=start, nrows=nrow(v)/ncol(x))
		}
		return(x)
	}	
)


.getTransientRows <- function(x, r, n=1) {
	stop()
#	reg = c(n, ncol(x))
#	off = c(r-1,0)
#	as.vector((rgdal::getRasterData(x@file@transient, region.dim=reg, offset=off)))
}

