\name{head}

\docType{methods}

\alias{head}
\alias{head,RasterLayer-method}
\alias{head,RasterStackBrick-method}
\alias{head,Spatial-method}
\alias{tail}
\alias{tail,RasterLayer-method}
\alias{tail,RasterStackBrick-method}
\alias{tail,Spatial-method}

\title{Show the head or tail of a Raster* object}

\description{
Show the head (first rows/columns) or tail (last rows/columns) of the cell values of a Raster* object. 
}

\usage{
head(x, ...) 
tail(x, ...) 
}

\arguments{
  \item{x}{Raster* object}
  \item{...}{Additional arguments: \code{rows=10} and \code{cols=20}, to set the maximum number of rows and columns that are shown. For RasterStack and RasterBrick objects there is an additional argument \code{lyrs} }
}

\value{
matrix
}

\seealso{
\code{\link{getValuesBlock}}
}



\examples{
r <- raster(nrow=25, ncol=25)
r[] = 1:ncell(r)
head(r)
tail(r, cols=10, rows=5)
}

\keyword{methods}
\keyword{spatial}

