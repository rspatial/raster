# Author: Robert J. Hijmans
# Date :  April 2009
# Version 0.9
# Licence GPL v3


setMethod("contour", signature(x='RasterLayer'), 
	function(x, maxpixels=100000, ...)  {
		x <- sampleRegular(x, maxpixels, asRaster=TRUE, useGDAL=TRUE)
		contour(x=xFromCol(x,1:ncol(x)), y=yFromRow(x, nrow(x):1), z=t((getValues(x, format='matrix'))[nrow(x):1,]), ...)
	}
)



rasterToContour <- function(x, maxpixels=100000, ...) {
	x <- sampleRegular(x, size=maxpixels, asRaster=TRUE, useGDAL=TRUE)
	cL <- grDevices::contourLines(x=xFromCol(x,1:ncol(x)), y=yFromRow(x, nrow(x):1), z=t((getValues(x, format='matrix'))[nrow(x):1,]), ...)
	
# The below was taken from ContourLines2SLDF(maptools), by Roger Bivand & Edzer Pebesma 
	.contourLines2LineList <- function(cL) {
		n <- length(cL)
		res <- vector(mode="list", length=n)
		for (i in 1:n) {
			crds <- cbind(cL[[i]][[2]], cL[[i]][[3]])
			res[[i]] <- sp::Line(coords=crds)
		}
		res
	}
	
    if (length(cL) < 1) stop("no contour lines")
    cLstack <- tapply(1:length(cL), sapply(cL, function(x) x[[1]]), function(x) x, simplify = FALSE)
    df <- data.frame(level = names(cLstack))
    m <- length(cLstack)
    res <- vector(mode = "list", length = m)
    IDs <- paste("C", 1:m, sep = "_")
    row.names(df) <- IDs
    for (i in 1:m) {
        res[[i]] <- sp::Lines(.contourLines2LineList(cL[cLstack[[i]]]), ID = IDs[i])
    }
    SL <- sp::SpatialLines(res,  proj4string= .getCRS((x)))
    sp::SpatialLinesDataFrame(SL, data = df)
	
}


filledContour <- function(x, y=1, maxpixels=100000, ...) {
	if (nlayers(x) > 1) {	
		y <- min(max(1, y), nlayers(x))
		x <- raster(x, y) 
	}
	x <- sampleRegular(x, maxpixels, asRaster=TRUE, useGDAL=TRUE)
	X <- xFromCol(x, 1:ncol(x))
	Y <- yFromRow(x, nrow(x):1)
	Z <- t( matrix( getValues(x), ncol=x@ncols, byrow=TRUE)[nrow(x):1,] )
	
	lonlat <- couldBeLonLat(x, warnings=FALSE)
	asp <- list(...)$asp
 	if (is.null(asp)) {
		if (lonlat) {
			ym <- mean(c(x@extent@ymax, x@extent@ymin))
			asp <- 1/cos((ym * pi)/180)
		} else {
			asp <- 1
		}		
		graphics::filled.contour(x=X,y=Y,z=Z,asp=asp,...)
	} else {
		graphics::filled.contour(x=X,y=Y,z=Z,...)
	}
}
