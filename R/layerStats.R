# Jonathan Greenberg and Robert Hijmans
# Date : April 2012
# Version 1.0
# Licence GPL v3

# Computation of the weighted covariance and (optionally) weighted means of bands in an Raster.
# based on code by Mort Canty


layerStats <- function(x, stat, w, asSample=TRUE, na.rm=FALSE, ...) {
	
	stat <- tolower(stat)
	stopifnot(stat %in% c('cov', 'weighted.cov', 'pearson'))
	stopifnot(is.logical(asSample) & !is.na(asSample))

	nl <- nlayers(x)
	n <- ncell(x)
	mat <- matrix(NA, nrow=nl, ncol=nl)
	colnames(mat) <- rownames(mat) <- names(x)

	pb <- pbCreate(nl^2, label='layerStats', ...)	
	
	if (stat == 'weighted.cov') {
		if (missing(w))	{
			stop('to compute weighted covariance a weights layer should be provided')
		}
		stopifnot( nlayers(w) == 1 )

		if (na.rm) {
		# a cell is set to NA if it is NA in any layer. That is not ideal, but easier and quicker
			nas <- calc(x, function(i) sum(i)) * w
			x <- mask(x, nas)
			w <- mask(w, nas)
		}

		sumw <- cellStats(w, stat='sum', na.rm=na.rm) 
		means <- cellStats(x * w, stat='sum', na.rm=na.rm) / sumw
		sumw <- sumw - asSample
		
		x <- (x - means) * sqrt(w)
		

		for(i in 1:nl) {
			for(j in i:nl) {
				r <- raster(x, layer=i) * raster(x,layer=j)
				v <- cellStats(r, stat='sum', na.rm=na.rm) / sumw
				mat[j,i] <- mat[i,j] <- v
				pbStep(pb)
			}
		}
		pbClose(pb)
		cov.w <- list(mat, means)
		names(cov.w) <- c("weigthed covariance", "weighted mean")
		return(cov.w)		
		
	} else if (stat == 'cov') {

		means <- cellStats(x, stat='mean', na.rm=na.rm) 
		x <- (x - means)
		
		for(i in 1:nl) {
			for(j in i:nl) {
				r <- raster(x, layer=i) * raster(x, layer=j)
				if (na.rm) {
					v <- cellStats(r, stat='sum', na.rm=na.rm) / (n - cellStats(r, stat='countNA') - asSample)
				} else {
					v <- cellStats(r, stat='sum', na.rm=na.rm) / (n - asSample)
				}
				mat[j,i] <- mat[i,j] <- v
				pbStep(pb)
			}
		}
		pbClose(pb)
		covar <- list(mat, means)
		names(covar) <- c("covariance", "mean")
		return(covar)		
		
	} else if (stat == 'pearson') {

		means <- cellStats(x, stat='mean', na.rm=na.rm) 
		sds <- cellStats(x, stat='sd', na.rm=na.rm) 
		x <- (x - means)
		
		for(i in 1:nl) {
			for(j in i:nl) {
				r <- raster(x, layer=i) * raster(x, layer=j)
				if (na.rm) {
					v <- cellStats(r, stat='sum', na.rm=na.rm) / ((n - cellStats(r, stat='countNA') - asSample) * sds[i] * sds[j])
				} else {
					v <- cellStats(r, stat='sum', na.rm=na.rm) / ((n - asSample) * sds[i] * sds[j])
				}
				mat[j,i] <- mat[i,j] <- v
				pbStep(pb)
			}
		}
		pbClose(pb)
		covar <- list(mat, means)
		names(covar) <- c("pearson correlation coefficient", "mean")
		return(covar)
		
	}
}


