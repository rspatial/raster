\name{erase}

\docType{methods}

\alias{erase}
\alias{erase,SpatialPolygons,SpatialPolygons-method}
\alias{erase,SpatialLines,SpatialPolygons-method}

\title{ 
Erase parts of a SpatialPolygons* or SpatialLines* object. The inverse of this can be done with \code{\link{intersect}}
}

\description{
Erase parts of a SpatialPolygons* or SpatialLines* object with a SpatialPolygons* object
}

\usage{
\S4method{erase}{SpatialPolygons,SpatialPolygons}(x, y, ...)
\S4method{erase}{SpatialLines,SpatialPolygons}(x, y, ...)
}

\arguments{
  \item{x}{SpatialPolygons or SpatialLines object}
  \item{y}{SpatialPolygons object}
  \item{...}{Additional arguments (none)}  
}

\value{
Spatial*
}

\seealso{The equivalent for raster data is \code{\link{mask}}}

\author{Robert J. Hijmans}

\examples{
if (require(rgdal) & require(rgeos)) {
# erase parts of polygons with other polygons
	p <- shapefile(system.file("external/lux.shp", package="raster"))
	b <- as(extent(6, 6.4, 49.75, 50), 'SpatialPolygons')
	projection(b) <- projection(p)
	e <- erase(p, b)
	plot(e)
	
	
# erase parts of lines with polygons	
	r <- raster(extent(p) +c(-.1,.1,-.1,.1), crs=crs(p))
	start <- xyFromCell(r, cellFromCol(r, 1))
	end <- xyFromCell(r, cellFromCol(r, ncol(r)))
	lines <- do.call(spLines, lapply(1:10, function(i)rbind(start[i,], end[i,])))
	crs(lines) <- crs(p)
	
	e2 <- erase(lines, p)
	plot(p)
	lines(lines, col='blue', lwd=4, lty=3)
	lines(e2, col='red', lwd=2)
}
}

\keyword{methods}
\keyword{spatial}

