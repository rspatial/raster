# Author: Robert J. Hijmans
# Date : October 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("all.equal")) {
	setGeneric("all.equal", function(target, current, ...)
		standardGeneric("all.equal"))
}	

setMethod("all.equal", c("Raster", "Raster"),
	function(target, current, values=TRUE, stopiffalse=FALSE, showwarning=TRUE, ...) { 
		compareRaster(target, current, ..., values=values, stopiffalse=stopiffalse, showwarning=showwarning)
	}
)


compareRaster <- function(x, ..., extent=TRUE, rowcol=TRUE, crs=TRUE, res=FALSE, orig=FALSE, rotation=TRUE, values=FALSE, tolerance, stopiffalse=TRUE, showwarning=FALSE) {

	if (missing(tolerance)) {
		tol <- .tolerance()
	} else {
		tol <- tolerance
	}
	
	result <- TRUE
	objects <- c(x, list(...))
	if (!isTRUE(length(objects) > 1)) {
		warning('There should be at least 2 Raster* objects to compare')
		return(result)
	}	
	minres <- min(res(objects[[1]]))
	proj1 <- .get_projection(objects[[1]])
	ext1 <- extent(objects[[1]])
	ncol1 <- ncol(objects[[1]])
	nrow1 <- nrow(objects[[1]])
	res1 <- res(objects[[1]])
	origin1 <- abs(origin(objects[[1]]))
	rot1 <- rotated(objects[[1]])	
	
	for (i in 2:length(objects)) { 
		if (extent) {
			if (!(isTRUE(all.equal(ext1, extent(objects[[i]]), tolerance=tol, scale=minres )))) {
				result <- FALSE
				if (stopiffalse) { stop('different extent') }
				if (showwarning) { warning('different extent') }
			}	
		}	
		if (rowcol) {
			if ( !(identical(ncol1, ncol(objects[[i]]))) ) {
				result <- FALSE
				if (stopiffalse) { stop('different number or columns') } 
				if (showwarning) { warning('different number or columns') } 
			}	
			if ( !(identical(nrow1, nrow(objects[[i]]))) ) {
				result <- FALSE
				if (stopiffalse) { stop('different number or rows') }
				if (showwarning) { warning('different number or rows') }
			}
		}
		if (crs) {
			thisproj <-.get_projection(objects[[i]])
			if (is.na(proj1)) {
				proj1 <- thisproj
			} else {
				crs <- try (compareCRS(proj1, thisproj, unknown=TRUE), silent=TRUE)
				if (inherits(crs, "try-error")) {
					if (stopiffalse) { stop("invalid CRS") }
					if (showwarning) { warning("invalid CRS") }
				} else if (!crs) {
					result <- FALSE
					if (stopiffalse) { stop("different CRS") }
					if (showwarning) { warning("different CRS") }
				}
			}
		}
		
# Can also check res through extent & rowcol
		if (res) {
			if (!(isTRUE(all.equal(res1, res(objects[[i]]), tolerance=tol, scale=minres)))) {
				result <- FALSE
				if (stopiffalse)  { stop('different resolution') }
				if (showwarning) { warning('different resolution') }
			}	
		}
# Can also check orig through extent & rowcol, but orig is useful for e.g. Merge(raster, raster)
		if (orig) {
			dif <- origin1 - abs(origin(objects[[i]]))
			if (!(isTRUE(all.equal(dif, c(0,0), tolerance=tol, scale=minres)))) {
				result <- FALSE
				if (stopiffalse) { stop('different origin') }
				if (showwarning) { warning('different origin') }
			}
		}
		
		if (rotation) {
			rot2 <- rotated(objects[[i]])
			if (rot1 | rot2) {
				if (rot1 != rot2) {
					if (stopiffalse) { stop('not all objects are rotated') }
					if (showwarning) { warning('not all objects are rotated') }
					result <- FALSE
				} else {
					test <- all(objects[[i]]@rotation@geotrans == objects[[1]]@rotation@geotrans)
					if (! test) {
						if (stopiffalse) { stop('rotations are different') }
						if (showwarning) { warning('rotations are different') }
						result <- FALSE
					}
				}
			}
		}
		
		if (values) {
			hv1 <- hasValues(objects[[1]])
			hvi <- hasValues(objects[[i]])
			if (hv1 != hvi) {
				if (stopiffalse) { stop('not all objects have values') }
				if (showwarning) { warning('not all objects have values') }
				result <- FALSE
			} else if (hv1 & hvi) { 
				if (canProcessInMemory(objects[[1]])) {
					test <- isTRUE(all.equal(getValues(objects[[1]]), getValues(objects[[i]])))
					if (! test) {
						if (stopiffalse) { stop('not all objects have the same values') }
						if (showwarning) { warning('not all objects have the same values') }
						result <- FALSE
					}	
				} else {
					tr <- blockSize(objects[[1]])
					for (j in 1:tr$n) {
						v1 <- getValues(objects[[1]], tr$row[j], tr$nrows[j])
						v2 <- getValues(objects[[i]], tr$row[j], tr$nrows[j])
						if (!isTRUE(all.equal(v1, v2))) {
							if (stopiffalse) { stop('not all objects have the same values') }
							if (showwarning) { warning('not all objects have the same values') }
							result <- FALSE
							break
						}
					}
				}
			}
		}
	}
	return(result)
}

