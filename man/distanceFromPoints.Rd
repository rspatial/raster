\name{distanceFromPoints}

\alias{distanceFromPoints}

\title{Distance from points}

\description{
The function calculates the distance from a set of points to all cells of a Raster* object.
 
The distance unit is in meters if the coordinate reference system (crs) of the Raster* object is (\code{+proj=longlat}) or assumed to be if the crs is \code{NA}. In all other cases it is in the units defined by the crs (which typically is meters). 
}

\usage{
distanceFromPoints(object, xy, filename='', ...) 
}

\arguments{
\item{object}{Raster object}
\item{xy}{matrix of x and y coordinates, or a SpatialPoints* object.}
\item{filename}{character. Optional filename for the output RasterLayer}
\item{...}{Additional arguments as for \code{\link{writeRaster}}}  
}


\details{
Distances for \code{longlat} data are computed on the WGS84 spheroid using GeographicLib (Karney, 2013)
}

\references{
C.F.F. Karney, 2013. Algorithms for geodesics, J. Geodesy 87: 43-55. \doi{10.1007/s00190-012-0578-z}.
}


\value{RasterLayer}

\seealso{ \code{\link{crs}}, \code{\link[raster]{distance}},  \code{\link[raster]{gridDistance}},  \code{\link[raster]{pointDistance}}  }


\examples{
r <- raster(ncol=36,nrow=18)
xy <- c(0,0)
d1 <- distanceFromPoints(r, xy) 
crs(r) = '+proj=utm +zone=12 +datum=WGS84'
d2 <- distanceFromPoints(r, xy) 
par(mfrow=c(1,2))
plot(d1)
plot(d2)
}

\keyword{spatial}
