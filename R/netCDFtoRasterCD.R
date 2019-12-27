# Author: Robert J. Hijmans
# Date: Aug 2009
# Version 1.0
# Licence GPL v3
# Aug 2012, adapted for use with ncdf4 library 


.doTime <- function(x, nc, zvar, dim3) {
	dodays <- TRUE
	dohours <- FALSE

	un <- nc$var[[zvar]]$dim[[dim3]]$units	
	if (substr(un, 1, 10) == "days since") { 
		startDate = as.Date(substr(un, 12, 22))
	} else {
		if (substr(un, 1, 11) == "hours since") { 
			dohours <- TRUE
		}
		dodays <- FALSE
	}
	if (dohours) {
		startTime <- substr(un, 13, 30)
		startTime <- strptime(startTime, "%Y-%m-%d %H:%M:%OS")
		time <- startTime + as.numeric(getZ(x)) * 3600
		time <- as.character(time)
		if (!is.na(time[1])) {
			x@z <- list(time)
			names(x@z) <- as.character('Date/time')
		}
	} else if (dodays) {
		# cal = nc$var[[zvar]]$dim[[dim3]]$calendar ?
		cal <- ncdf4::ncatt_get(nc, "time", "calendar")
		if (! cal$hasatt ) {
			greg <- TRUE
		} else {
			cal <- cal$value
			if (cal =='gregorian' | cal =='proleptic_gregorian' | cal=='standard') {
				greg <- TRUE
			} else if (cal == 'noleap' | cal == '365 day' | cal == '365_day') { 
				greg <- FALSE
				nday <- 365
			} else if (cal == '360_day') { 
				greg <- FALSE
				nday <- 360
			} else {
				greg <- TRUE
				warning('assuming a standard calender:', cal)
			}
		}
		time <- getZ(x)
		if (greg) {
			time <- as.Date(time, origin=startDate)
		} else {
			startyear <-  as.numeric( format(startDate, "%Y") )
			startmonth <- as.numeric( format(startDate, "%m") )
			startday <- as.numeric( format(startDate, "%d") )
			year <- trunc( as.numeric(time)/nday )
			doy <- (time - (year * nday))
			origin <- paste(year+startyear, "-", startmonth, "-", startday, sep='')
			time <- as.Date(doy, origin=origin)		
		}
		x@z <- list(time)
		names(x@z) <- 'Date'
	}
	return(x)
}



.dimNames <- function(nc) {
	n <- nc$dim
	nams <- vector(length=n)
	if (n > 0) {
		for (i in 1:n) {
			nams[i] <- nc$dim[[i]]$name
		}
	}
	return(nams)
}


.varName <- function(nc, varname='', warn=TRUE) {
	n <- nc$nvars
	dims <- vars <- vector(length=n)
	if (n > 0) {
		for (i in 1:n) {
			vars[i] <- nc$var[[i]]$name
			dims[i] <- nc$var[[i]]$ndims
		}
		vars <- vars[dims > 1]
		dims <- dims[dims > 1]
	}

	if (varname=='') { 
		nv <- length(vars)
		if (nv == 0) {
			return('z')
		} 
		
		if (nv  == 1) {
			varname <- vars
		} else {
			varname <- vars[which.max(dims)]
			if (warn) {
				if (sum(dims == max(dims)) > 1) {
					vars <- vars[dims==max(dims)]
					warning('varname used is: ', varname, '\nIf that is not correct, you can set it to one of: ', paste(vars, collapse=", ") )
				}
			}
		}
	}

	zvar <- which(varname == vars)
	if (length(zvar) == 0) {
		stop('varname: ', varname, ' does not exist in the file. Select one from:\n', paste(vars, collapse=", ") )
	}
	return(varname)
}


.rasterObjectFromCDF <- function(filename, varname='', band=NA, type='RasterLayer', lvar, level=0, warn=TRUE, dims=1:3, crs=NA, stopIfNotEqualSpaced=TRUE, ...) {

	stopifnot(requireNamespace("ncdf4"))
	stopifnot(type %in% c('RasterLayer', "RasterBrick"))
	
	nc <- ncdf4::nc_open(filename, suppress_dimvals = TRUE)
	on.exit( ncdf4::nc_close(nc) )		
	conv <- ncdf4::ncatt_get(nc, 0, "Conventions")
		
	# assuming > "CF-1.0"
	
	zvar <- .varName(nc, varname, warn=warn)
	# datatype <- .getRasterDTypeFromCDF( nc$var[[zvar]]$prec )
	dim3 <- dims[3]
	ndims <- nc$var[[zvar]]$ndims
	
	if (ndims== 1) { 
		
		return(.rasterObjectFromCDF_GMT(nc))
		
	} else if (ndims == 4) { 
		if (type != 'RasterQuadBrick') {
			if (missing(lvar)) {
				nlevs3 <- nc$var[[zvar]]$dim[[3]]$len
				nlevs4 <- nc$var[[zvar]]$dim[[4]]$len
				if (nlevs3 > 1 & nlevs4 == 1) {
					lvar <- 4
				} else {
					lvar <- 3
				}
			}
			nlevs <- nc$var[[zvar]]$dim[[lvar]]$len
			if (level <=0 ) {
				level <- 1
				# perhaps detect case where lvar should be 4?
				#https://stackoverflow.com/questions/56261199/extracting-all-levels-from-netcdf-file-in-r/
				if (nlevs > 1) {
					warning('"level" set to 1 (there are ', nlevs, ' levels)')
				}
			} else {
				oldlevel <- level <- round(level)
				level <- max(1, min(level, nlevs))
				if (oldlevel != level) {
					warning('level set to: ', level)
				}
			}
			if (lvar == 4) { 
				dim3 <- 3 
			} else { 
				dim3 <- 4 
			}
		}
	} else if (ndims > 4) { 
		warning(zvar, ' has more than 4 dimensions, I do not know what to do with these data')
	}
	
	ncols <- nc$var[[zvar]]$dim[[dims[1]]]$len
	nrows <- nc$var[[zvar]]$dim[[dims[2]]]$len

	## to allow suppress_dimvals
	## xx <- nc$var[[zvar]]$dim[[dims[1]]]$vals
	xx <- try(ncdf4::ncvar_get(nc, nc$var[[zvar]]$dim[[dims[1]]]$name), silent = TRUE)
	if (inherits(xx, "try-error")) {
	  xx <- seq_len(nc$var[[zvar]]$dim[[dims[1]]]$len)
	}
	
	rs <- xx[-length(xx)] - xx[-1]
	if (! isTRUE ( all.equal( min(rs), max(rs), tolerance=0.025, scale= abs(min(rs))) ) ) {
		if (is.na(stopIfNotEqualSpaced)) {
			warning('cells are not equally spaced; you should extract values as points') 
		} else if (stopIfNotEqualSpaced) {
			stop('cells are not equally spaced; you should extract values as points') 
		}
	}
	
	
	xrange <- c(min(xx), max(xx))
	resx <- (xrange[2] - xrange[1]) / (ncols-1)
	rm(xx)

	## to allow suppress_dimvals
  ##	yy <- nc$var[[zvar]]$dim[[dims[2]]]$vals
	yy <- try(ncdf4::ncvar_get(nc, nc$var[[zvar]]$dim[[dims[2]]]$name), silent = TRUE)
	if (inherits(yy, "try-error")) {
	  yy <- seq_len(nc$var[[zvar]]$dim[[dims[2]]]$len)
	}
	
	rs <- yy[-length(yy)] - yy[-1]
	if (! isTRUE ( all.equal( min(rs), max(rs), tolerance=0.025, scale= abs(min(rs))) ) ) {
		if (is.na(stopIfNotEqualSpaced)) {
			warning('cells are not equally spaced; you should extract values as points') 
		} else if (stopIfNotEqualSpaced) {
			stop('cells are not equally spaced; you should extract values as points') 
		}
	}
	yrange <- c(min(yy), max(yy))
	resy <- (yrange[2] - yrange[1]) / (nrows-1)

	if (yy[1] > yy[length(yy)]) { toptobottom  <- FALSE
	} else { toptobottom <- TRUE }

	rm(yy)

	xrange[1] <- xrange[1] - 0.5 * resx
	xrange[2] <- xrange[2] + 0.5 * resx
	yrange[1] <- yrange[1] - 0.5 * resy
	yrange[2] <- yrange[2] + 0.5 * resy
 
	long_name <- zvar
	unit <- ''

	natest <- ncdf4::ncatt_get(nc, zvar, "_FillValue")
	natest2 <- ncdf4::ncatt_get(nc, zvar, "missing_value")		
	
	prj <- NA
	minv <- maxv <- NULL
	a <- ncdf4::ncatt_get(nc, zvar, "min")
	if (a$hasatt) { minv <- a$value }
	a <- ncdf4::ncatt_get(nc, zvar, "max")
	if (a$hasatt) { maxv <- a$value }

	a <- ncdf4::ncatt_get(nc, zvar, "long_name")
	if (a$hasatt) { long_name <- a$value }
	a <- ncdf4::ncatt_get(nc, zvar, "units")
	if (a$hasatt) { unit <- a$value }
	a <- ncdf4::ncatt_get(nc, zvar, "grid_mapping")
	if ( a$hasatt ) { 
		gridmap  <- a$value 
		try(atts <- ncdf4::ncatt_get(nc, gridmap), silent=TRUE)
		try(prj <- .getCRSfromGridMap4(atts), silent=TRUE)
	}		
	if (is.na(prj)) {
		if ((tolower(substr(nc$var[[zvar]]$dim[[dims[1]]]$name, 1, 3)) == 'lon')  &
		   ( tolower(substr(nc$var[[zvar]]$dim[[dims[2]]]$name, 1, 3)) == 'lat' ) ) {
				if ( yrange[1] > -91 | yrange[2] < 91 ) {
					if ( xrange[1] > -181 | xrange[2] < 181 ) {
						prj <- '+proj=longlat +datum=WGS84'
					} else if ( xrange[1] > -1 | xrange[2] < 361 ) {
						prj <- '+proj=longlat +lon_wrap=180 +datum=WGS84'
					}
				}
			
		}
	} 
		
	crs <- .getProj(prj, crs)
				
	if (type == 'RasterLayer') {
		r <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows, crs=crs)
		names(r) <- long_name
	} else if (type == 'RasterBrick') {
		r <- brick(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows, crs=crs)
		r@title <- long_name
	} else if (type == 'RasterQuadBrick') {
		r <- .quad(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows, crs=crs)
		r@title <- long_name	
		if (lvar == 4) { 
			dim3 <- 3 
			step3 <- 4
		} else { 
			dim3 <- 4 
			step3 <- 3
		}
		r@nlevels <- nc$var[[zvar]]$dim[[dim3]]$len
		r@steps  <- nc$var[[zvar]]$dim[[step3]]$len
	}
	
	r@file@name <- filename
	r@file@toptobottom <- toptobottom
	if (type == 'RasterLayer') { r@file@dimreadorder <- dims[1:2]
	} else {r@file@dimreadorder <- dims }
	r@data@unit <- unit
	
	
	attr(r@data, "zvar") <- zvar
	attr(r@data, "dim3") <- dim3
	attr(r@data, "level") <- level
	
	r@file@driver <- "netcdf"	
	
	if (natest$hasatt) { 
		r@file@nodatavalue <- as.numeric(natest$value)
	} else if (natest2$hasatt) { 
		r@file@nodatavalue <- as.numeric(natest2$value)
	}
	r@data@fromdisk <- TRUE
	
	if (ndims == 2) {
		nbands <- 1
	} else {
		nbands <- nc$var[[zvar]]$dim[[dim3]]$len
		r@file@nbands <- nbands
		## to allow suppress_dimvals
  	#	r@z <- list( nc$var[[zvar]]$dim[[dim3]]$vals )
		dim3_vals <- try(ncdf4::ncvar_get(nc, nc$var[[zvar]]$dim[[dim3]]$name), silent = TRUE)
		if (inherits(dim3_vals, "try-error")) {
		  dim3_vals <- seq_len(nc$var[[zvar]]$dim[[dim3]]$len)
		}
		r@z <- list(dim3_vals)
		if ( nc$var[[zvar]]$dim[[dim3]]$name == 'time' ) {
			try( r <- .doTime(r, nc, zvar, dim3) )
		} else {
			vname <- nc$var[[zvar]]$dim[[dim3]]$name
			vunit <- nc$var[[zvar]]$dim[[dim3]]$units
			names(r@z) <- paste0(vname, " (", vunit, ")")
		}
	}

	if (length(ndims)== 2 & type != 'RasterLayer') { 
		warning('cannot make a RasterBrick from data that has only two dimensions (no time step), returning a RasterLayer instead')	
	} 

	
	if (type == 'RasterLayer') {
		if (is.null(band) | is.na(band)) {
			if (ndims > 2) { 
				stop(zvar, ' has multiple layers, provide a "band" value between 1 and ', nc$var[[zvar]]$dim[[dim3]]$len)
			} 
		} else {
			if (length(band) > 1) {
				stop('A RasterLayer can only have a single band. You can use a RasterBrick instead')
			}		
			if (is.na(band)) {
				r@data@band <- as.integer(1)
			} else {
				band <- as.integer(band)
				if ( band > nbands(r) ) {
					stop(paste("The band number is too high. It should be between 1 and", nbands))
				} 
				if ( band < 1) { 
					stop(paste("band should be 1 or higher"))		
				}			
				r@data@band <- band
			}
			r@z <- list( getZ(r)[r@data@band] )
			if (!(is.null(minv) | is.null(maxv))) {
				r@data@min <- minv[band]
				r@data@max <- maxv[band]
				r@data@haveminmax <- TRUE
			}
			
		} 

	} else {
		r@data@nlayers <- r@file@nbands
		try( names(r) <- as.character(r@z[[1]]), silent=TRUE )
		if (!(is.null(minv) | is.null(maxv))) {
			r@data@min <- minv
			r@data@max <- maxv
			r@data@haveminmax <- TRUE
		} else {
			r@data@min <- rep(Inf, r@file@nbands)
			r@data@max <- rep(-Inf, r@file@nbands)
		}
		
	}
	
	return(r)
}

