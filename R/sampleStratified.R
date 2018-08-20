# Author: Robert J. Hijmans
# Date : June 2012
# Version 2.0
# Licence GPL v3


if (!isGeneric("sampleStratified")) {
	setGeneric("sampleStratified", function(x, size, ...)
		standardGeneric("sampleStratified"))
}	


setMethod('sampleStratified', signature(x='RasterLayer'), 
function(x, size, exp=10, na.rm=TRUE, xy=FALSE, ext=NULL, sp=FALSE, ...) {

	stopifnot(hasValues(x)) 
	
	size <- round(size)
	stopifnot(size <= ncell(x))
	stopifnot(size > 0)
		
	if (!is.null(ext)) {
		oldx <- raster(x)
		x <- crop(x, ext)
	}
	
	if (canProcessInMemory(x)) {
	
		v <- cbind(1:ncell(x), round(getValues(x)))
		if (na.rm) {
			v <- v[!is.na(v[,2]), ]
		}
		f <- table(v[,2], useNA='ifany')
		f <- cbind(as.integer(names(f)), f)
	
		ys <- list()
		
		for (i in 1:nrow(f)) {
			if (is.na(f[i,1])) {
				y <- v[is.na(v[, 2]),  ,drop=FALSE]
			} else {
				y <- v[v[, 2] == f[i,1], ,drop=FALSE]
			}
			if (nrow(y) < size) {
				warning("only ", nrow(y), " cells found for stratum ", f[i,1])
			} else {
				if (nrow(y) > size) {
					y <- y[sample(nrow(y), size),  ,drop=FALSE]
				}
			}
			# bug fix by Antoine Stevens
			ys[[i]] <- y
		}
		
	} else {
	
		# unique would suffice, unless to check whether a sample _can_ be obtained for a stratum
		f <- freq(x)
		if (na.rm) {
			na <- which(is.na(f[,1]))
			if (length(na) > 0) {
				f <- f[-na, ,drop=FALSE]
			}
		}

		exp <- max(1, exp)
		ss <- exp * size * nrow(f)
		if (ss < 1000) {
			ss <- 1000
		}
		if (ss > ncell(x)) {
			ss <- ncell(x)
		}
			
		sr <- sampleRandom(x, ss, na.rm=na.rm, ext=NULL, cells=TRUE, rowcol=FALSE, sp=FALSE)
	
		ys <- list()
		for (i in seq_len(nrow(f))) {
			y <- sr[sr[, 2] == f[i,1], ,drop=FALSE]
			if (nrow(y) == 0) {
				warning("no samples found for value: ", i, ". Perhaps increase the value of 'ext'")
			} else {
				if (nrow(y) > size) {
					y <- y[sample(nrow(y), size),  ,drop=FALSE]
				} 
				ys[[i]] <- y
			}
		}
	}
	
	res <- do.call(rbind, ys)
	colnames(res) <- c('cell', names(x))
	
	ta <- tapply(res[,1], res[,2], length) 
	tanm <- names(ta)[which(ta < size)]
	
	if (length(tanm)== 1) {
		warning('fewer samples than requested found for stratum: ', tanm)
	} else if (length(tanm) > 1) {
		warning('fewer samples than requested found for strata: ', paste(tanm, collapse=', '))
	}

	if (!is.null(ext)) {
		pts <- xyFromCell(x, res[,1])
		res[,1] <- cellFromXY(oldx, pts)
		if (xy) {
			res <- cbind(res[,1,drop=FALSE], pts, res[,2,drop=FALSE])
		} 
	} else if (xy) {
		pts <- xyFromCell(x, res[,1])
		res <- cbind(res[,1,drop=FALSE], pts, res[,2,drop=FALSE])
	}	
	
	if (sp) {
		if (!xy & is.null(ext)) {
		  pts <- xyFromCell(x, res[,1])
		}
		res <- SpatialPointsDataFrame(pts, data.frame(res), proj4string=projection(x, asText=FALSE))
	}
	return(res)
}
)




