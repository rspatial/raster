# Author: Robert J. Hijmans, Paul Hiemstra, Steven Mosher
# Date :  January 2009
# Version 0.9
# Licence GPL v3


.pointsToRaster <- function(xy, r, field, fun='last', background=NA, mask=FALSE, update=FALSE, updateValue='all', filename="", na.rm=TRUE, ...) {

	rs <- raster(r)
	
	if (mask & update) { 
		stop('use either "mask=TRUE" OR "update=TRUE", or neither')
	} else if (mask) { 
		oldraster <- r
	} else if (update) {
		oldraster <- r 
		if (!is.numeric(updateValue)) {
			if (is.na(updateValue)) {
				updateValue <- 'NA'
			} else if (!(updateValue == 'NA' | updateValue == '!NA' | updateValue == 'all')) {
				stop('updateValue should be either "all", "NA", "!NA"')
			}
		} 
	}
	

	if (is.character(fun)) {
		if (!(fun %in% c('first', 'last', 'sum', 'min', 'max', 'count'))) {
			stop('invalid value for fun')
		}
		if (fun == 'sum') {
			fun <- sum
		} else if (fun == 'min') {
			fun <- min
		} else if (fun == 'max') {
			fun <- max
		} else {
			if (na.rm) {
				if (fun == 'first') {
					fun <- function(x, ...) { 
						# stats::na.omit(x[1]) 
						# fix by Daniel Schlapfer
							stats::na.omit(x)[1]
						}
						
				} else if (fun == 'last') {
					fun <- function(x, ...) { x <- stats::na.omit(x); x[length(x)] }
				} else if (fun == 'count') {
					fun <- function(x, ...) length(stats::na.omit(x))
				}
			} else {
				if (fun == 'first') {
					fun <- function(x, ...) { x[1] }
				} else if (fun == 'last') {
					fun <- function(x, ...) { 
						# x[length(x)] 
						# fix by Daniel Schlapfer
						x <- stats::na.omit(x)
						if (length(x) > 0) {
							x[length(x)]
						} else { 
							NA
						}

					}
				} else if (fun == 'count') {
					fun <- function(x, ...) length(x)
				}
			}
		}
	}
	
	points <- .pointsToMatrix(xy)

	field <- .getPutVals(xy, field, nrow(points), mask)

	xy <- points
	
	nres <- max(length(fun(1)), length(fun(1:5)))
	ncols <- 1
	
	if (NCOL(field) > 1) {
		if (nres > 1) stop('Either use a single function for "fun", or a single vector for "field"')
		nres <- ncols <- ncol(field)
	} else {
		if (is.atomic(field) & length(field)==1) {
			field <- rep(field, dim(xy)[1])
		}
		if (nrow(xy) != NROW(field)) {
			stop('number of points does not match the number of fields')
		}
	}
	
	
	cells <- cellFromXY(rs, xy)
	
#	todisk <- TRUE
	todisk <- FALSE
	if (!canProcessInMemory(rs, 2 * nres))  {
		if (filename == '') {
			filename <- rasterTmpFile()
		}
		todisk <- TRUE
	}	
	
	
	if (todisk) {
		rows <- rowFromCell(rs, cells)
		cols <- colFromCell(rs, cells)
		xyarc <- cbind(xy, rows, cols, field)
		urows <- unique(rows)
#		urows <- urows[order(urows)]
		if (nres==1) {
			dna <- vector(length=ncol(rs))
			dna[] <- background
		} else {
			rs <- brick(rs)  #  return a'RasterBrick'
			rs@data@nlayers <- nres
			if (ncols > 1) { names(rs) <- colnames(field) }
			dna <- matrix(background, nrow=ncol(rs), ncol=nres)
			datacols <- 5:ncol(xyarc)
		}
		pb <- pbCreate(nrow(rs), ...)
		rs <- writeStart(rs, filename=filename, ...)
		for (r in 1:rs@nrows) {
			d <- dna
			if (r %in% urows) {
				ss <- subset(xyarc, xyarc[,3] == r)
				#ucols <- unique(ss[,5])
				#for (c in 1:length(ucols)) {
				#	sss <- subset(ss, ss[,5] == ucols[c] )
				#	d[ucols[c]] <- fun(sss[,3])	
				#}
				
				if (ncols > 1) {
					v <- aggregate(ss[,datacols,drop=FALSE], list(ss[,4]), fun, na.rm=na.rm)
					cells <- as.numeric(v[,1])
					d[cells, ] <- as.matrix(v)[,-1]
				} else {
					v <- tapply(ss[,5], ss[,4], fun, na.rm=na.rm)
					cells <- as.numeric(rownames(v))
					if (nres > 1) {
						v <- as.matrix(v)
						v <- t(apply(v, 1, function(x) x[[1]]))  # Reshape the data if more than one value is returned by 'fun'
						d[cells, ] <- v
					} else {
						d[cells] <- v
					}
				}
			}
			
# need to check if nlayers matches ncols (how many layers returned?)
			if (mask) {
				oldvals <- getValues(oldraster, r)
				ind <- which(is.na(d))
				oldvals[ind] <- NA
				d <- oldvals
			} else if (update) {
				oldvals <- getValues(oldraster, r)
				if (updateValue == "all") {
					ind <- which(!is.na(d))
				} else if (updateValue == "zero") {
					ind <- which(oldvals==0 & !is.na(d))
				} else if (updateValue == "NA") {
					ind <- which(is.na(oldvals))
				} else {
					ind <- which(!is.na(oldvals) & !is.na(d))
				}
				oldvals[ind] <- d[ind]
				d <- oldvals
			}
			
			rs <- writeValues(rs, d, r) 
			pbStep(pb, r)
		}
		
		rs <- writeStop(rs)
		pbClose(pb)
		
	} else {
	
		v <- aggregate(field, list(cells), fun, na.rm=na.rm)
		cells <- as.numeric(v[,1])
		v <- as.matrix(v)[,-1,drop=FALSE]
		
		if(class(v[1]) == "list") {
			v <- t(apply(v, 1, function(x) x[[1]]))  # Reshape the data if more than one value is returned by 'fun'
		}

		if (ncol(v) > 1) { 
			vv <- matrix(background, nrow=ncell(rs), ncol=dim(v)[2])
			vv[cells, ] <- v
		    rs <- brick(rs)  #  return a'RasterBrick'
		} else {
			vv <- 1:ncell(rs)
			vv[] <- background
			vv[cells] <- v
		}
		
		if (mask) {
			oldvals <- getValues(oldraster)
			ind <- which(is.na(vv))
			oldvals[ind] <- NA
			vv <- oldvals
		} else if (update) {
			oldvals <- getValues(oldraster)
			if (updateValue == "all") {
				ind <- which(!is.na(vv))
			} else if (updateValue == "zero") {
				ind <- which(oldvals==0 & !is.na(vv))
			} else if (updateValue == "NA") {
				ind <- which(is.na(oldvals))
			} else {
				ind <- which(!is.na(oldvals) & !is.na(vv))
			}
			oldvals[ind] <- vv[ind]
			vv <- oldvals
		}
	
		rs <- setValues(rs, vv)
		if (ncols > 1) {
			cn <- colnames(field)
			if (! is.null(cn)) {
				names(rs) <- cn
			}	
		}

		if (filename != "") {
			rs <- writeRaster(rs, filename=filename, ...)
		}
	}
	return(rs)	
}
