\name{rowFromCell}

\alias{rowFromCell}
\alias{rowFromCell,BasicRaster,numeric-method}
\alias{colFromCell}
\alias{colFromCell,BasicRaster,numeric-method}
\alias{rowColFromCell}
\alias{rowColFromCell,BasicRaster,numeric-method}

  
\title{Row or column number from a cell number}

\description{
These functions get the row and/or column number from a cell number of a Raster* object)
}

\usage{
colFromCell(object, cell)
rowFromCell(object, cell)
rowColFromCell(object, cell)
}

\arguments{
  \item{object}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{cell}{cell number(s)}
}
  
\details{
The colFromCell and similar functions accept a single value, or a vector or list of these values,
Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom.
The last cell number equals the number of cells of the Raster* object.
}

\value{
  row of column number(s) 
}

\seealso{ \code{\link[raster]{cellFrom}} }




\examples{
r <- raster(ncols=10, nrows=10)
colFromCell(r, c(5,15))
rowFromCell(r, c(5,15))
rowColFromCell(r, c(5,15))
}

\keyword{spatial}
