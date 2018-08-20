\name{readAll}

\alias{readAll,RasterLayer-method}
\alias{readAll,RasterStack-method}
\alias{readAll,RasterBrick-method}

\alias{readAll}

\title{Read values from disk}

\description{
Read all values from a raster file associated with a Raster* object into memory. This function should normally not be used. In most cases \code{\link[raster]{getValues}} or \code{\link[raster]{getValuesBlock}} is more appropriate as \code{readAll} will fail when there is no file associated with the RasterLayer (values may only exist in memory). 
}


\usage{
readAll(object)
}


\arguments{
  \item{object}{a Raster* object}
}


\seealso{ \code{\link[raster]{getValues}}, \code{\link[raster]{getValuesBlock}}, \code{\link[raster]{extract}}}



\examples{
r <- raster(system.file("external/test.grd", package="raster"))
r <- readAll(r)
}

\keyword{classes}
\keyword{spatial}
