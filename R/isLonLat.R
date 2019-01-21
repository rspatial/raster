# Author: Robert J. Hijmans
# Date :  October 2008
# Version 0.9
# Licence GPL v3



.isGlobalLonLat <- function(x) {
	res <- FALSE
	tolerance <- 0.1
	scale <- xres(x)
	if (isTRUE(all.equal(xmin(x), -180, tolerance=tolerance, scale=scale)) & 
		isTRUE(all.equal(xmax(x),  180, tolerance=tolerance, scale=scale))) {
		if (couldBeLonLat(x, warnings=FALSE)) {
 			res <- TRUE
		}
	}
	res
}


.couldBeLonLat <- function(...) {
	couldBeLonLat(...)
}

couldBeLonLat <- function(x, warnings=TRUE) {
	crsLL <- isLonLat(x)
	crsNA <- is.na(projection(x))
	e <- extent(x)
	extLL <- (e@xmin > -365 & e@xmax < 365 & e@ymin > -90.1 & e@ymax < 90.1) 
	if (extLL & isTRUE(crsLL)) { 
		return(TRUE)
	} else if (extLL & crsNA) {
		if (warnings) {
			warning('CRS is NA. Assuming it is longitude/latitude')
		}
		return(TRUE)
	} else if (isTRUE(crsLL)) {
		if (warnings) {
			warning('raster has a longitude/latitude CRS, but coordinates do not match that')
		}
		return(TRUE)
	} else {
		return(FALSE) 	
	}
}




setMethod('isLonLat', signature(x='Spatial'), 
	function(x){
		isLonLat(projection(x))
    }
)


setMethod('isLonLat', signature(x='BasicRaster'), 
# copied from the SP package (slightly adapted)
#author:
# ...
	function(x){
		p4str <- projection(x)
		if (is.na(p4str) || nchar(p4str) == 0) {
			return(FALSE)
		} 
		res <- grep("longlat", p4str, fixed = TRUE)
		if (length(res) == 0) {
			return(FALSE)
		} else {
			return(TRUE)
		}
    }
)

setMethod('isLonLat', signature(x='character'), 
# copied from the SP package (slightly adapted)
#author:
# ...
	function(x){
		res <- grep("longlat", x, fixed = TRUE)
		if (length(res) == 0) {
			return(FALSE)
		} else {
			return(TRUE)
		}
    }
)



setMethod('isLonLat', signature(x='CRS'), 
# copied from the SP package (slightly adapted)
#author:
# ...
	function(x){
		if (is.na(x@projargs)) { 
			return(FALSE)
		} else {
			s <- trim(x@projargs)
		}	
		if (is.na(s) || nchar(s) == 0) {
			return(FALSE)
		} 
		s <- gsub(" ", "", s)
		res1 <- grep("longlat", s)
		res2 <- grep("+init=epsg:4326", s)
		res <- c(res1, res2)
		if (length(res) == 0) {
			return(FALSE)
		} else {
			return(TRUE)
		}
    }
)

setMethod('isLonLat', signature(x='ANY'), 
	function(x){
		isLonLat(as.character(x))
    }
)
