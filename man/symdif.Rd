\name{symdif}

\docType{methods}

\alias{symdif}
\alias{symdif,SpatialPolygons,SpatialPolygons-method}

\title{ 
Symetrical difference
}

\description{
Symetrical difference of SpatialPolygons* objects
}

\usage{
\S4method{symdif}{SpatialPolygons,SpatialPolygons}(x, y, ...)
}

\arguments{
  \item{x}{SpatialPolygons* object}
  \item{y}{SpatialPolygons* object}
  \item{...}{Additional SpatialPolygons* object(s)}  
}

\value{
SpatialPolygons*
}

\seealso{
\code{\link{erase}} 
}

\examples{
#SpatialPolygons
if (require(rgdal) & require(rgeos)) {
	p <- shapefile(system.file("external/lux.shp", package="raster"))
	b <- as(extent(6, 6.4, 49.75, 50), 'SpatialPolygons')
	crs(b) <- crs(p)
	sd <- symdif(p, b)
	plot(sd, col='red')
}
}

\keyword{methods}
\keyword{spatial}

