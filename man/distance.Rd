\name{distance}

\alias{distance}
\alias{distance,RasterLayer,missing-method}
\alias{distance,RasterLayer,RasterLayer-method}
\alias{distance,Spatial,Spatial-method}

\title{Distance}

\description{
For a single \code{RasterLayer} (\code{y} is missing) this method computes the distance, for all cells that are \code{NA}, to the nearest cell that is not \code{NA}.
The distance unit is in meters if the RasterLayer is not projected (\code{+proj=longlat}) and in map units (typically also meters) when it is projected.
 
If two \code{RasterLayer} objects are provided, the cell-value distances are computed. 
If two \code{Spatial} vector type objects are provided, the distances between pairs of geographic object are computed. 
}

\usage{
\S4method{distance}{RasterLayer,missing}(x, y, filename='', doEdge=TRUE, ...)
\S4method{distance}{RasterLayer,RasterLayer}(x, y, ...)
\S4method{distance}{Spatial,Spatial}(x, y, ...)
}

\arguments{
\item{x}{RasterLayer object}
\item{y}{missing, RasterLayer or Spatial object}
\item{filename}{Character. Filename for the output RasterLayer (optional)}
\item{doEdge}{Logical. If \code{TRUE}, the \code{\link{boundaries}} function is called first. This may be efficient in cases where you compute the distance to large blobs. Calling \code{boundaries} determines the edge cells that matter for distance computation}
\item{...}{Additional arguments as for \code{\link{writeRaster}}}
}




\value{RasterLayer}

\seealso{
\code{\link[raster]{distanceFromPoints}},  \code{\link[raster]{gridDistance}},  \code{\link[raster]{pointDistance}}  

See the \code{gdistance} package for more advanced distances, and the \code{geosphere} package for great-circle distances (and more) between points in longitude/latitude coordinates.
}


\examples{
r <- raster(ncol=36,nrow=18)
values(r) <- NA
r[500] <- 1
dist <- distance(r) 
#plot(dist / 1000)
}

\keyword{spatial}
