\name{layerize}

\docType{methods}

\alias{layerize}
\alias{layerize,RasterLayer,missing-method}
\alias{layerize,RasterLayer,RasterLayer-method}

\title{Layerize}

\description{
Create a RasterBrick with a Boolean layer for each class (value, or subset of the values) in a RasterLayer. For example, if the cell values of a RasterLayer indicate what vegetation type they are, this function will create a layer (presence/absence; dummy variable) for each of these classes. Classes and cell values are always truncated to integers.

You can supply a second spatially overlapping RasterLayer with larger cells (do not use smaller cells!). In this case the cell values are counts for each class. A similar result might be obtained more efficiently by using layerize with a single RasterLayer followed by \code{\link{aggregate}(x,  , sum)}.
}

\usage{
\S4method{layerize}{RasterLayer,missing}(x, classes=NULL, falseNA=FALSE, filename='', ...)

\S4method{layerize}{RasterLayer,RasterLayer}(x, y, classes=NULL, filename='', ...)
}

\arguments{
  \item{x}{RasterLayer}
  \item{y}{RasterLayer or missing}
  \item{classes}{numeric. The values (classes) for which layers should be made. If \code{NULL} all classes are used}
  \item{falseNA}{logical. If \code{TRUE}, cells that are not of the class represented by a layer are \code{NA} rather then \code{FALSE}}
  \item{filename}{character. Output filename (optional)}
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}
}

\value{
RasterBrick
}

\examples{
r <- raster(nrow=20, ncol=20)
values(r) <- c(rep(NA, 50), rep(1:5, 70))
b <- layerize(r)

r2 <- raster(nrow=5, ncol=5)
b2 <- layerize(r, r2)
}

\keyword{spatial}
