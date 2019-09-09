\name{flip}

\docType{methods}

\alias{flip}
\alias{flip,RasterLayer-method}
\alias{flip,RasterStackBrick-method}
  
\title{Flip}

\description{
Flip the values of a Raster* object by inverting the order of the rows (direction=y) or the columns direction='x'.
}

\usage{
flip(x, direction, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{direction}{Character. 'y' or 'x'; or 1 (=x) or 2 (=y)}
   \item{...}{Additional arguments as for \code{\link{writeRaster}}}  
}

\value{
RasterLayer or RasterBrick
}


\seealso{ transpose: \code{\link{t}}, \code{\link[raster]{rotate}} }



\examples{
r <- raster(nrow=18, ncol=36)
m <- matrix(1:ncell(r), nrow=18)
values(r) <- as.vector(t(m))
rx <- flip(r, direction='x')
values(r) <- as.vector(m)
ry <- flip(r, direction='y')
}

\keyword{spatial}
