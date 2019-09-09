\name{buffer}

\alias{buffer}
\alias{buffer,RasterLayer-method}
\alias{buffer,Spatial-method}


\title{buffer}

\description{
Calculate a buffer around all cells that are not \code{NA} or around SpatialPoints, Lines, or Polygons.
 
Note that the distance unit of the buffer \code{width} parameter is meters if the RasterLayer is not projected (\code{+proj=longlat}), and in map units (typically also meters) when it is projected.

Except for SpatialLines and SpatialPolygons that are currently handled by rgeos, and can only deal with planar coordinate reference systems.
}

\usage{
\S4method{buffer}{RasterLayer}(x, width=0, filename='', doEdge=FALSE, ...)

\S4method{buffer}{Spatial}(x, width=1, dissolve=TRUE, ...)
}

\arguments{
\item{x}{RasterLayer or Spatial* object}
\item{width}{numeric > 0. Unit is meter if \code{x} has a longitude/latitude CRS, or mapunits in other cases}
\item{filename}{character. Filename for the output RasterLayer (optional)}
\item{doEdge}{logical. If \code{TRUE}, the \code{\link{boundaries}} function is called first. This may be efficient in cases where you compute a buffer around very large areas because \code{boundaries} determines the edge cells that matter for distance computation}
\item{dissolve}{logical. If \code{TRUE}, buffer geometries of overlapping polygons are dissolved and all geometries are aggregated and attributes (the data.frame) are dropped}
\item{...}{Additional arguments as for \code{\link{writeRaster}}}
}


\value{RasterLayer or SpatialPolygons* object}

\seealso{
\code{\link[raster]{distance}},  \code{\link[raster]{gridDistance}},  \code{\link[raster]{pointDistance}}  
}


\examples{
r <- raster(ncol=36,nrow=18)
values(r) <- NA
r[500] <- 1
b <- buffer(r, width=5000000) 
#plot(b)
}

\keyword{spatial}
