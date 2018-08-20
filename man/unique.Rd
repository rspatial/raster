\name{unique}

\docType{methods}

\alias{unique}
\alias{unique,RasterLayer,missing-method}
\alias{unique,RasterStackBrick,missing-method}

\title{Unique values}

\description{
This function returns the unique values in a RasterLayer, or the unique combinations of values in a multi-layer raster object.
}

\usage{
\S4method{unique}{RasterLayer,missing}(x, incomparables=FALSE, ...) 

\S4method{unique}{RasterStackBrick,missing}(x, incomparables=FALSE, ...) 
}

\arguments{
  \item{x}{Raster object}
  \item{incomparables}{ignored. Must be missing}
  \item{...}{additional arguments. One implemented: \code{progress}, as in \code{\link{writeRaster}}}  
}

\seealso{ \code{\link[base]{unique}} }

\value{
vector or matrix
}



\examples{
r <- raster(ncol=10, nrow=10)
r[] <- round(runif(ncell(r))*10)
unique(r)
unique(stack(r, round(r/2)))
}

\keyword{spatial}
