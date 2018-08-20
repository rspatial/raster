\name{rotate}

\docType{methods}

\alias{rotate}
\alias{rotate,Raster-method}

  
\title{Rotate}

\description{
Rotate a Raster* object that has x coordinates (longitude) from 0 to 360, to standard coordinates between -180 and 180 degrees. Longitude between 0 and 360 is frequently used in global climate models.
}

\usage{
\S4method{rotate}{Raster}(x, filename='', ...)
}

\arguments{
\item{x}{Raster* object}
\item{filename}{character. Output filename (optional)}
\item{...}{additional arguments as for \code{\link{writeRaster}}}  
}

\value{
RasterLayer or a RasterBrick object
}


\seealso{ \code{\link[raster]{flip}} }


\examples{

r <- raster(nrow=18, ncol=36)
m <- matrix(1:ncell(r), nrow=18)
r[] <- as.vector(t(m))
extent(r) <- extent(0, 360, -90, 90)
rr <- rotate(r)
}

\keyword{spatial}
