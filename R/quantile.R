# Author: Robert J. Hijmans 
# r.hijmans@gmail.com
# Date : October 2008
# Licence GPL v3


setMethod('quantile', signature(x='Raster'), 
	function(x, ..., na.rm=TRUE, ncells=NULL) {
		if (is.null(ncells)) {
			v <- try ( getValues(x) )
			if (inherits(v, "try-error")) {
				stop('raster too large. You can sample it with argument "ncells"')
			}
		} else {
			if (ncells >= ncell(x)) {
				v <- try ( getValues(x) )
			} else {
				v <- try ( sampleRandom(x, ncells) ) 
			}
			if (inherits(v, "try-error")) {
				stop('ncells too large')
			}
		}
		#if (na.rm) {
		#	v <- stats::na.omit(v)
		#}
		if (nlayers(x)==1) {
			return(quantile(v, ..., na.rm=na.rm))
		} else {
			# t(apply(v, 2, quantile, na.rm=TRUE))

			q <- stats::quantile(v[,1], ..., na.rm=na.rm)
			for (i in 2:nlayers(x)) {
				q <- rbind(q, stats::quantile(v[,i], ..., na.rm=na.rm))
			}
			rownames(q) <- names(x)
			return(q)
		}
	}
)

