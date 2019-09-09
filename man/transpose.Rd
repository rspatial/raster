\name{transpose}

\docType{methods}

\alias{t}
\alias{t,RasterLayer-method}
\alias{t,RasterStackBrick-method}
  
\title{Transpose}

\description{
Transpose a Raster* object
}

\usage{
t(x)
}

\arguments{
 \item{x}{a Raster* object}
}

\value{
RasterLayer or RasterBrick
}

\seealso{ transpose: \code{\link{flip}, \link[raster]{rotate}} }



\examples{
r <- raster(nrow=18, ncol=36)
values(r) <- 1:ncell(r)
rt <- t(r)
}

\keyword{spatial}
