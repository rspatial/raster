\name{addLayer}

\alias{addLayer}
\alias{addLayer,Raster-method}
\alias{dropLayer}
\alias{dropLayer,RasterStack-method}
\alias{dropLayer,RasterBrick-method}

\title{Add or drop a layer}

\description{
Add a layer to a Raster* object or drop a layer from a RasterStack or RasterBrick. The object returned is always a RasterStack (unless nothing to add or drop was provided, in which case the original object is returned).
}

\usage{
addLayer(x, ...) 
dropLayer(x, i) 
}

\arguments{
  \item{x}{Raster* object}
  \item{i}{integer. Indices of the layers to be dropped}
  \item{...}{Additional arguments. The layers to add for addLayer. None for dropLayer)}
}


\value{
RasterStack
}

\seealso{ \code{\link[raster]{subset}}}

\examples{
file <- system.file("external/test.grd", package="raster")
s <- stack(file, file, file)
r <- raster(file)
s <- addLayer(s, r/2, r*2)
s
s <- dropLayer(s, c(3, 5))
nlayers(s)
}

\keyword{ spatial }
