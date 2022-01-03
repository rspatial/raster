\name{direction}

\alias{direction}
\alias{direction,RasterLayer-method}

\title{Direction}

\description{
The direction (azimuth) to or from the nearest cell that is not \code{NA}. The direction unit is in radians, unless you use argument \code{degrees=TRUE}. 
}

\usage{
\S4method{direction}{RasterLayer}(x, filename='', degrees=FALSE, from=FALSE, doEdge=FALSE, ...) 
}

\arguments{
\item{x}{RasterLayer object}
\item{filename}{Character. Output filename (optional)}
\item{degrees}{Logical. If \code{FALSE} (the default) the unit of direction is radians.}
\item{from}{Logical. Default is \code{FALSE}. If \code{TRUE}, the direction from (instead of to)  the nearest cell that is not \code{NA} is returned}
\item{doEdge}{Logical. If \code{TRUE}, the \code{\link{boundaries}} function is called first. This may be efficient in cases where you compute the distance to large blobs. Calling \code{boundaries} determines the edge cells that matter for direction computation}
\item{...}{Additional arguments as for \code{\link{writeRaster}}}
}



\value{RasterLayer}


\seealso{
\code{\link[raster]{distance}},  \code{\link[raster]{gridDistance}}

For the direction between (longitude/latitude) points, see the \code{azimuth} function in the \code{geosphere} package 
}


\examples{
r <- raster(ncol=36,nrow=18)
values(r) <- NA
r[306] <- 1
b <- direction(r) 
#plot(b)
}

\keyword{spatial}
