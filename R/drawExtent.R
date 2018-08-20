# R function for the raster package
# Author: Robert J. Hijmans
# Date : January 2009, December 2011
# Version 1.0
# Licence GPL v3



drawExtent <- function(show=TRUE, col="red") {
	if (show) {
		loc1 <- graphics::locator(n=1, type="p", pch='+', col=col)
	} else {
		loc1 <- graphics::locator(n=1)	
	}
	loc2 <- graphics::locator(n=1)
	loc <- rbind(unlist(loc1), unlist(loc2))
	e <- extent(min(loc[,'x']), max(loc[,'x']), min(loc[,'y']), max(loc[,'y']))
	if (e@xmin == e@xmax) {
		e@xmin <- e@xmin - 0.0000001
		e@xmax <- e@xmax + 0.0000001
	}
	if (e@ymin == e@ymax) {
		e@ymin <- e@ymin - 0.0000001
		e@ymax <- e@ymax + 0.0000001
	}
	if (show) {
		p <- rbind(c(e@xmin, e@ymin), c(e@xmin, e@ymax), c(e@xmax, e@ymax), c(e@xmax, e@ymin), c(e@xmin, e@ymin) )
		lines(p, col=col)
	}
	return(e)
}
