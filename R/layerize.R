# Author: Robert J. Hijmans
# Date : August 2012
# Version 1.0
# Licence GPL v3




setMethod('layerize', signature(x='RasterLayer', y='missing'), 
	function(x, classes=NULL, falseNA=FALSE, filename='', ...) {
		
		doC <- list(...)$doC
		if (is.null(doC)) doC <- TRUE		

		if (is.null(classes)) {
			classes <- as.integer( sort(unique(x)) )
		} else {
			classes <- as.integer(classes) 
		}
		
		out <- raster(x)
		if (length(classes) > 1) {
			out <- brick(out, nl=length(classes))
		}
		names(out) <- classes
		
		if (canProcessInMemory(out)) {

			v <- as.integer(getValues(x))
			if (doC) {
				v <- .layerize(v, as.integer(classes), falseNA)
				v <- matrix(v, ncol=length(classes))
			} else {
				v <- t( apply(matrix(v), 1, function(x) x == classes) )
				if (falseNA) {
					v[!v] <- NA
				}
			}
# alternative approach (assuming sorted classes)
# alternative approach (assuming sorted classes)
#			vv <- cbind(1:length(v), as.integer(as.factor(v)))
#			if (falseNA) {
#				v <- matrix(NA, nrow=ncell(out), ncol=nlayers(out))
#			} else {
#				v <- matrix(0, nrow=ncell(out), ncol=nlayers(out))
#			}
#			v[vv] <- 1
				
			out <- setValues(out, v*1)
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
			return(out)
		}
		
# else to disk		

##			out <- writeStart(out, filename=filename, datatype='INT2S', ...)
#		} else {
		out <- writeStart(out, filename=filename, ...)
#		}

		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='layerize', ...)

		#fNA <- as.integer(falseNA)
		if (doC) {
			for (i in 1:tr$n) {
				v <- as.integer(getValues(x, tr$row[i], tr$nrows[i]))
				v <- .layerize(v, classes, falseNA)
				v <- matrix(v, ncol=length(classes))
				out <- writeValues(out, v*1, tr$row[i])
				pbStep(pb, i) 
			}
		} else {
			for (i in 1:tr$n) {
				v <- getValues(x, tr$row[i], tr$nrows[i]) 
				v <- t( apply(matrix(v, ncol=1), 1, function(x) x == classes) )
				if (falseNA) {
					v[!v] <- NA
				}
				out <- writeValues(out, v*1, tr$row[i])
				pbStep(pb, i) 
			}
		}

		pbClose(pb)
		writeStop(out)	
	}
)



setMethod('layerize', signature(x='RasterLayer', y='RasterLayer'), 
function(x, y, classes=NULL, filename='', ...) { 

	resx <- res(x)
	resy <- res(y)
	if (! all( resy > resx) ) {
		stop("x and y resolution of object y should be (much) larger than that of object x")
	}
	
	int <- intersect(extent(x), extent(y))
	if (is.null(int)) {
		return(raster(y))
	}

	if (is.null(classes)) {
		classes <- as.integer( sort(unique(x)))
	}	
	out <- raster(y)
	if (length(classes) > 1) {
		out <- brick(out, nl=length(classes))
	}
	names(out) <- paste('count_', as.character(classes), sep='')
	
	if (canProcessInMemory( out )) {
		b <- crop(x, int)
		xy <- xyFromCell(b, 1:ncell(b))
		mc <- cellFromXY(out, xy)
		b <- as.integer(getValues(b))
		if (!is.null(classes)) {
			b[! b %in% classes] <- NA
		}	
		v <- table(mc, b)
		cells <- as.integer(rownames(v))
		m <- match(cells, 1:ncell(out))
		cn <- as.integer(colnames(v))
		res <- matrix(NA, nrow=ncell(out), ncol=length(cn))
		for (i in 1:length(cn)) {
			 res[m,i] <- v[,i]
		}
		
		names(out) <- paste('count_', as.character(cn), sep='')
		out <- setValues(out, res)
		
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		}
		return(out)
	} 
	#  else 

	
	out <- writeStart(out, filename=filename, ...)
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, label='layerize', ...)
	for(i in 1:tr$n) {		
		e <- extent(xmin(y), xmax(y), yFromRow(y, tr$row[i]+tr$nrows[i]-1)  - 0.5 * yres(y), yFromRow(y, tr$row[i])+0.5 * yres(y))
		int <- intersect(e, extent(x)) 

		res <- matrix(NA, nrow=tr$nrows[i] * ncol(y), ncol=length(classes))
		if (!is.null(int)) {
			b <- crop(x, int)
			xy <- xyFromCell(b, 1:ncell(b))
			mc <- cellFromXY(y, xy)
			v <- table(mc, as.integer(getValues(b)))
			cells <- as.integer(rownames(v))
			modcells <- cellFromRowCol(y, tr$row[i], 1) : cellFromRowCol(y, tr$row[i]+ tr$nrows[i]-1, ncol(y))
			m <- match(cells, modcells)
			cn <- as.integer(colnames(v))
			mm <- match(cn, classes)
			for (j in 1:length(cn)) {
				res[, mm[j]] <- v[, j]
			}
		}	
		out <- writeValues(out, res, tr$row[i])
	}		
	out <- writeStop(out)
	pbClose(pb)
	out	
}
)

