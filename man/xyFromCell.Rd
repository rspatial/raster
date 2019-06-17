\name{xyFromCell}

\alias{xFromCol}
\alias{xFromCol,Raster,numeric-method}
\alias{xFromCol,Raster,missing-method}
\alias{yFromRow}
\alias{yFromRow,Raster,numeric-method}
\alias{yFromRow,Raster,missing-method}
\alias{xFromCell}
\alias{xFromCell,Raster,numeric-method}
\alias{yFromCell}
\alias{yFromCell,Raster,numeric-method}
\alias{xyFromCell}
\alias{xyFromCell,BasicRaster-method}
\alias{xyFromCell,BasicRaster,ANY-method}
\alias{xyFromCell,Raster-method}
\alias{xyFromCell,Raster-method}
\alias{coordinates}
\alias{coordinates,Raster-method}


\title{Coordinates from a row, column or cell number}

\description{
These functions get coordinates of the center of raster cells for a row, column, or cell number of a Raster* object.
}

\usage{
\S4method{xFromCol}{Raster,numeric}(object, col)
\S4method{yFromRow}{Raster,numeric}(object, row)
\S4method{xFromCell}{Raster,numeric}(object, cell)
\S4method{yFromCell}{Raster,numeric}(object, cell)
\S4method{xyFromCell}{BasicRaster,ANY}(object, cell, spatial=FALSE, ...)
\S4method{coordinates}{Raster}(obj, ...)
}


\arguments{
  \item{object}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{col}{column number; or vector of column numbers. If missing, the x coordinates for all columns are returned}
  \item{row}{row number; or vector of row numbers. If missing, the y coordinates for all rows are returned}
  \item{cell}{cell number(s)}
  \item{spatial}{If \code{spatial=TRUE}, \code{xyFromCell} returns a SpatialPoints object instead of a matrix}
  \item{...}{additional arguments. None implemented}
  \item{obj}{Raster object}
}
  
\details{
Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom.
The last cell number equals the number of cells of the Raster* object.
}

\value{
xFromCol, yFromCol, xFromCell, yFromCell: vector of x or y coordinates 

xyFromCell: matrix(x,y) with coordinate pairs 

coordinates: xy coordinates for all cells
}


\seealso{
\code{\link{cellFromXY}}
}

\examples{
#using a new default raster (1 degree global)
r <- raster()
xFromCol(r, c(1, 120, 180))
yFromRow(r, 90)
xyFromCell(r, 10000)
xyFromCell(r, c(0, 1, 32581, ncell(r), ncell(r)+1))

#using a file from disk
r <- raster(system.file("external/test.grd", package="raster"))
r
cellFromXY(r, c(180000, 330000))
#xy for corners of a raster:
xyFromCell(r, c(1, ncol(r), ncell(r)-ncol(r)+1, ncell(r)))
}

\keyword{spatial}
