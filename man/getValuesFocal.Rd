
\name{getValuesFocal}

\alias{getValuesFocal}
\alias{getValuesFocal,Raster-method}
\alias{getValuesFocal,Raster,missing,missing,numeric-method}
\alias{getValuesFocal,Raster,numeric,numeric,numeric-method}
  
\title{Get focal raster cell values}

\description{
This function returns a matrix (or matrices) for all focal values of a number of rows of a Raster* object}

\usage{
\S4method{getValuesFocal}{Raster}(x, row, nrows, ngb, names=FALSE, padValue=NA, array=FALSE, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{row}{Numeric. Row number, should be between 1 and nrow(x). Can be omitted to get all rows}
 \item{nrows}{Numeric. Number of rows, should be a positive integer smaller than \code{row+nrow(x)}. Should be omitted if \code{row} is omitted}
 \item{ngb}{Neighbourhood size. Either a single integer or a vector of two integers \code{c(nrow, ncol)}}
 \item{names}{logical. If \code{TRUE}, the matrix returned has row and column names}
 \item{padValue}{numeric. The value of the cells of the "padded" rows and columns. That is 'virtual' values for cells within a neighbourhood, but outside the raster} 
 \item{array}{logical. If \code{TRUE} and \code{x} has multiple layers, an array is returned in stead of a list of matrices}
 \item{...}{additional arguments (none implemented)}
}


\value{
If \code{x} has a single layer, a matrix with one row for each focal cell, and one column for each neighbourhood cell around it.

If \code{x} has multiple layers, an array (if \code{array=TRUE}) or a list of such matrices (one list element (matrix) for each layer)
}

\seealso{ \code{\link{getValues}, \link{focal}} }


\examples{
r <- raster(nr=5, nc=5, crs='+proj=utm +zone=12')
values(r) <- 1:25
as.matrix(r)
getValuesFocal(r, row=1, nrows=2, ngb=3, names=TRUE)
getValuesFocal(stack(r,r), row=1, nrows=1, ngb=3, names=TRUE, array=TRUE)
}

\keyword{spatial}
\keyword{methods}
