# Author: Robert J. Hijmans
# Date: June 2008
# Version 1.0
# Licence GPL v3


if (!isGeneric("summary")) {
	setGeneric("summary", function(object, ...)
		standardGeneric("summary"))
}	



setMethod('summary', signature(object='RasterLayer'), 
	function(object, maxsamp=100000, ...) {
		
		if ( inMemory(object) ) {
			sm <- as.matrix( stats::quantile( values(object), na.rm=TRUE) )
			sm <- c(sm, sum(is.na( values(object) )))
			
		} else if ( fromDisk(object) ) {
			if (ncell(object) > maxsamp) {
				v <- sampleRegular(object, maxsamp)
				nas <- round(sum(is.na(v)) * ncell(object) / maxsamp)

				warning(paste('summary is an estimate based on a sample of ', maxsamp, ' cells (', round(100*maxsamp/ncell(object), 2), '% of all cells)\n', sep=''))

			} else {
				v <- getValues(object)
				nas <- sum(is.na(v))
			}
			sm <- stats::quantile(v, na.rm=TRUE)
			sm <- c(sm, nas)
			
		} else {
			sm <- NA
		}
		values <- matrix(sm, ncol=1, nrow=6)
		rownames(values) <- c('Min.', '1st Qu.', 'Median', '3rd Qu.', 'Max.', "NA's")
		colnames(values) <- names(object)
		return(values)
	}	
)





setMethod('summary', signature(object='RasterStackBrick'), 
	function(object, maxsamp=100000, ...) {
			
		if ( inMemory(object) ) {
		
			sm <- apply(object@data@values, 2, quantile, na.rm=TRUE)
			nas <- apply(is.na(object@data@values), 2, sum)
			values <- rbind(sm, nas)

		} else if (  fromDisk(object) ) {
			
			nc <- ncell(object)
			if (nc > maxsamp) {
				v <- sampleRegular(object, maxsamp)
				nas <- round(apply(is.na(v), 2, sum) * nc / maxsamp)
				warning(paste('summary is an estimate based on a sample of ', maxsamp, ' cells (', round(100*maxsamp/nc, 2), '% of all cells)\n', sep=''))
				
			} else {
				v <- getValues(object)
				nas <- apply(is.na(v), 2, sum)
			}

			sm <- apply(v, 2, quantile, na.rm=T)
			values <- rbind(sm, nas)
			
		} else {
			stop('no cell values associated with this RasterBrick')
		}
		rownames(values) <- c('Min.', '1st Qu.', 'Median', '3rd Qu.', 'Max.', "NA's")
		return(values)
	}
)	

