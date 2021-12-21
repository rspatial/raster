# Author: Robert J. Hijmans
# Date : October 2009
# Version 0.9
# Licence GPL v3


.rasterFromASCIIFile <- function(filename, offset=6, crs="", ...) {
	
	offset <- as.integer(offset)
	stopifnot(offset > 2)
	
	splitasc <- function(s) {
		s <- trim(s)
		spl <- unlist(strsplit(s, ''), use.names = FALSE)
		pos <- which(spl==' ')[1]
		first <- substr(s, 1, (pos-1))
		second <- substr(s, (pos+1), nchar(s))
		return(trim(c(first, second)))
	}
	
	filename <- trim(filename)
    if (!file.exists(filename)) { stop(paste(filename, " does not exist")) }
	con <- file(filename, "rt")
	lines <- readLines(con, n=offset)
	close(con)
	ini <- lapply(lines, splitasc) 
	ini <- matrix(unlist(ini, use.names = FALSE), ncol=2, byrow=TRUE)
	
	ini[,1] = toupper(ini[,1]) 
	
	suppressWarnings( test <- sum(as.numeric(ini[,1]), na.rm=TRUE) > 0 )
	if (test) {
		m <- 'The header of this file appears to be incorrect: there are numbers where there should be keywords'
		if (offset != 6) {
			m <- paste(m, '\n  Are you using a wrong offset?', sep='')
		} 
		stop(m)
	}
	
	
	nodataval <- xn <- yn <- d <- nr <- nc <- xc <- yc <- NA
	for (i in 1:nrow(ini)) {
		if (ini[i,1] == "NCOLS") { nc <- as.integer(ini[i,2])
		} else if (ini[i,1] == "NROWS") { nr <- as.integer(ini[i,2])
		} else if (ini[i,1] == "XLLCORNER") { xn <- as.numeric(ini[i,2])
		} else if (ini[i,1] == "XLLCENTER") { xc <- as.numeric(ini[i,2])
		} else if (ini[i,1] == "YLLCORNER") { yn <- as.numeric(ini[i,2])
		} else if (ini[i,1] == "YLLCENTER") { yc <- as.numeric(ini[i,2])
		} else if (ini[i,1] == "CELLSIZE") { d <- as.numeric(ini[i,2])
		} else if (ini[i,1] == "NODATA_VALUE") { try (nodataval <- as.numeric(ini[i,2]), silent=TRUE)
		} else if (ini[i,1] == "NODATA") { try (nodataval <- as.numeric(ini[i,2]), silent=TRUE)		
		}
    }  
	
	if (is.na(nr)) stop('"NROWS" not detected') 
	if (is.na(nc)) stop('"NCOLS" not detected')

	if (is.na(nodataval)) {
		warning('"NODATA_VALUE" not detected. Setting it to -Inf\n  You can set it to another value with function "NAvalue"')
		nodataval <- -Inf
	}
	
	offwarn <- FALSE
	if (is.na(d)) { 
		warning('"CELLSIZE" not detected. Setting it to 1.');
		offwarn = TRUE
		d <- 1 
	} else if (d==0) {
		warning('"CELLSIZE" is reported as zero. Setting it to 1.');
		d <- 1 
	}
	d <- abs(d)

	if (is.na(xn)) { 
		if (is.na(xc)) { 
			warning('"XLLCORNER" tag not detected. Setting it to 0.')
			offwarn = TRUE
			xn <- 0 
		} else {
			xn <- xc - 0.5 * d
		}
	}
	
	if (is.na(yn)) { 
		if (is.na(yc)) { 
			warning('"YLLCORNER" tag not detected. Setting it to 0.');
			offwarn = TRUE
			yn <- 0
		} else {
			yn <- yc - 0.5 * d
		} 	
	}
	if (offwarn) {
		m <- 'The georeference of this object is probably wrong\n'
		if (offset != 6) {
			m <- paste(m, '  Are you using a wrong offset? Proceed with caution!\n', sep='')
		} 
		warning(m)
	}
	
	xx <- xn + nc * d
	yx <- yn + nr * d

	x <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs='')	

	x@data@fromdisk <- TRUE
	x@file@offset <- offset
	x@file@driver <- 'ascii'
	x@file@nodatavalue <- nodataval
    x@file@name <- filename
	
	
	if (!is.na(crs)) {
		projection(x) <- .CRS()
	}
	
	return(x)
}


