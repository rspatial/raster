# Robert Hijmans
# Based on maptools:dotsInPolys by Roger Bivand


.dotdensity <- function(p, field, x=1, type="regular", seed=0, sp=FALSE, ...) {
	set.seed(seed)
	stopifnot(inherits(p, 'SpatialPolygons'))
    n <- length(p)
    if (n < 1) return(invisible(NULL))
	f <- tolower(type)
	stopifnot(type %in% c('regular', 'random'))  

	if (inherits(p, 'SpatialPolygonsDataFrame')) {
		if (is.numeric(field)) {
			if (length(field)==1) {
				field <- round(field)
				stopifnot(field > 0 & field <= ncol(p))
				field <- p@data[, field]
			} else {
				stopifnot(length(field)==length(p))
			}
		} else if (is.character(field)) {
			stopifnot(field %in% names(p))
			field <- p@data[, field]
		}
	} else {
		stopifnot(is.numeric(field))
		stopifnot(length(field)==length(p))
	}
	x <- x[1]
	stopifnot(x > 0)
	d <- round(field / x)
	d[d < 1] <- 0
	d[is.na(d)] <- 0
	
    res <- vector(mode = "list", length = n)
    for (i in 1:n) {
		if (d[i] > 0) {
			ires <- try (spsample(p[i, ], d[i], type=f), silent=TRUE  )
			if (class(ires) == 'try-error') {
				print(paste('error, ', d[i]))
				ires <- NULL
			}
			if (!is.null(ires)) {
				res[[i]] <- cbind(coordinates(ires), id=i)
			}
		}
    }
    res <- do.call("rbind", res)
	colnames(res)[1:2] <- c('x', 'y')
	if (sp) {
		res <- data.frame(res)
		coordinates(res) <- ~ x+y 
		crs(res) <- crs(p)
	}
	res
}


