\name{atan2}

\alias{atan2,Raster,Raster-method}
\alias{atan2}

\title{Two argument arc-tangent}

\description{
For Raster* objects x and y, atan2(y, x) returns the angle in radians for the tangent y/x, handling the case when x is zero. See \code{\link[base]{Trig}}

See \code{\link[raster]{Math-methods}} for other trigonometric and mathematical functions that can be used with Raster* objects.
}

\usage{
atan2(y, x) 
}

\arguments{
  \item{y}{Raster* object}
  \item{x}{Raster* object}
}

\seealso{ \code{\link[raster]{Math-methods}} }


\examples{
r1 <- r2 <- raster(nrow=10, ncol=10)
values(r1) <- (runif(ncell(r1))-0.5) * 10
values(r2) <- (runif(ncell(r1))-0.5) * 10
atan2(r1, r2)
}

\keyword{methods}
\keyword{math}
