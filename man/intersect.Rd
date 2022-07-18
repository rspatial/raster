\name{intersect}

\docType{methods}

\alias{intersect}

\alias{intersect,Extent,ANY-method}
\alias{intersect,Raster,ANY-method}

\alias{intersect,SpatialPoints,ANY-method}

\alias{intersect,SpatialPolygons,SpatialPolygons-method}
\alias{intersect,SpatialPolygons,SpatialLines-method}
\alias{intersect,SpatialPolygons,SpatialPoints-method}
\alias{intersect,SpatialPolygons,ANY-method}

\alias{intersect,SpatialLines,SpatialPolygons-method}
\alias{intersect,SpatialLines,SpatialLines-method}

\title{ 
Intersect
}

\description{
It depends on the classes of the \code{x} and \code{y} what is returned. 

If \code{x} is a Raster* object the extent of \code{y} is used, irrespective of the class of \code{y}, and a Raster* is returned. This is equivalent to \code{\link{crop}}. 

If \code{x} is a Spatial* object, a new Spatial* object is returned. If \code{x} or \code{y} has a data.frame, these are also returned (after merging if necessary) as part of a Spatial*DataFrame. 

Intersecting SpatialPoints* with SpatialPoints* uses the extent (bounding box) of \code{y} to get the intersection. Intersecting of SpatialPoints* and SpatialLines* is not supported because of numerical inaccuracies with that. You can use \code{\link{buffer}}, to create SpatialPoygons* from SpatialLines* and use that in intersect.
}


\usage{
\S4method{intersect}{Extent,ANY}(x, y)

\S4method{intersect}{Raster,ANY}(x, y)

\S4method{intersect}{SpatialPoints,ANY}(x, y)

\S4method{intersect}{SpatialPolygons,SpatialPolygons}(x, y)

\S4method{intersect}{SpatialPolygons,SpatialLines}(x, y)

\S4method{intersect}{SpatialPolygons,SpatialPoints}(x, y)

\S4method{intersect}{SpatialLines,SpatialPolygons}(x, y)

\S4method{intersect}{SpatialLines,SpatialLines}(x, y)
}

\arguments{
  \item{x}{Extent, Raster*, SpatialPolygons*, SpatialLines* or SpatialPoints* object}
  \item{y}{same as for \code{x}}
} 

\value{
if \code{x} is an Extent object: Extent

if \code{x} is a Raster* object: Raster*

if \code{x} is a SpatialPoints* object: SpatialPoints*

if \code{x} is a SpatialPolygons* object: SpatialPolygons*

if \code{x} is a SpatialLines* object and if \code{y} is a SpatialLines* object: SpatialPoints*

if \code{x} is a SpatialLines* object and if \code{y} is a SpatialPolygons* object: SpatialLines*
}

\seealso{
\code{\link{union}, \link[raster]{extent}, \link{crop}}
}

\examples{
e1 <- extent(-10, 10, -20, 20)
e2 <- extent(0, 20, -40, 5)
intersect(e1, e2)

#SpatialPolygons
p <- shapefile(system.file("external/lux.shp", package="raster"))
b <- as(extent(6, 6.4, 49.75, 50), 'SpatialPolygons')
projection(b) <- projection(p)
i <- intersect(p, b)
plot(p)
plot(b, add=TRUE, col='red')
plot(i, add=TRUE, col='blue', lwd=2)

}


\keyword{methods}
\keyword{spatial}

