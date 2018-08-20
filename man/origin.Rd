\name{origin}

\alias{origin}
\alias{origin,BasicRaster-method}
\alias{origin<-}
\alias{origin<-,BasicRaster-method}

  
\title{Origin}

\description{
Origin returns (or sets) the coordinates of the point of origin of a Raster* object. This is the point closest to (0, 0) that you could get if you moved towards that point in steps of the x and y resolution.
}

\usage{
origin(x, ...)
origin(x) <- value
}

\arguments{
  \item{x}{Raster* object}
  \item{value}{numeric vector of lenght 1 or 2}
  \item{...}{additional arguments. None implemented}  
}


\value{
A vector of two numbers (x and y coordinates), or a changed origin for \code{x}.
}

\seealso{ \code{\link[raster]{extent}}}


\examples{
r <- raster(xmn=-0.5, xmx = 9.5, ncols=10)
origin(r)
r
origin(r) <- 0
r
}

\keyword{spatial}
