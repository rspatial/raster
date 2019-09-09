\name{stackSelect}

\alias{stackSelect}
\alias{stackSelect,RasterStackBrick,Raster-method}


\title{Select cell values from a multi-layer Raster* object}

\description{
Use a Raster* object to select cell values from different layers in a multi-layer Raster* object. The object to select values \code{y} should have cell values between \code{1} and \code{nlayers(x)}. The values of \code{y} are rounded. 

See \code{\link{extract}} for extraction of values by cell, point, or otherwise.
}

\usage{
\S4method{stackSelect}{RasterStackBrick,Raster}(x, y, recycle=FALSE, type='index', filename='', ...)
}

\arguments{
  \item{x}{RasterStack or RasterBrick object}
  \item{y}{Raster* object}
  \item{recycle}{Logical. Recursively select values (default = \code{FALSE}. Only relevant if \code{y} has multiple layers. E.g. if \code{x} has 12 layers, and \code{y} has 4 layers, the indices of the \code{y} layers are used three times.}
  \item{type}{Character. Only relevant when \code{recycle=TRUE}. Can be 'index' or 'truefalse'. If it is 'index', the cell values of \code{y} should represent layer numbers. If it is 'truefalse' layer numbers are indicated by 0 (not used, NA returned) and 1 (used)}
  \item{filename}{Character. Output filename (optional)}
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}
}

\value{
Raster* object
}

\seealso{\code{\link{stackApply}}, \code{\link{extract}}}



\examples{
r <- raster(ncol=10, nrow=10, vals=1)
s <- stack(r, r+2, r+5)
values(r) <- round((runif(ncell(r)))*3)
x <- stackSelect(s, r)
}

\keyword{methods}
\keyword{spatial}

