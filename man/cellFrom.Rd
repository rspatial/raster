\name{cellFrom}

\alias{cellFromRowCol}
\alias{cellFromRowCol,BasicRaster,numeric,numeric-method}
\alias{colFromX}
\alias{colFromX,BasicRaster,numeric-method}
\alias{rowFromY}
\alias{rowFromY,BasicRaster,numeric-method}
\alias{cellFromXY}
\alias{cellFromXY,BasicRaster-method}

\alias{cellFromRow}
\alias{cellFromCol}
\alias{cellFromRowColCombine}
\alias{cellFromRowColCombine,BasicRaster,numeric,numeric-method}
\alias{fourCellsFromXY}
\alias{cellFromLine}
\alias{cellFromPolygon}


  
\title{Get cell, row, or column number}

\description{
Get cell number(s) of a Raster* object from row and/or column numbers.   
Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom.
The last cell number equals the number of cells of the Raster* object.
}

\usage{
cellFromRowCol(object, row, col, ...)
cellFromRowColCombine(object, row, col, ...)
cellFromRow(object, rownr)
cellFromCol(object, colnr)
colFromX(object, x)
rowFromY(object, y)
cellFromXY(object, xy)
cellFromLine(object, lns)
cellFromPolygon(object, p, weights=FALSE)
fourCellsFromXY(object, xy, duplicates=TRUE) 
}

\arguments{
  \item{object}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{colnr}{column number; or vector of column numbers}
  \item{rownr}{row number; or vector of row numbers}
  \item{col}{column number; or vector of column numbers}
  \item{row}{row number; or vector of row numbers}
  \item{x}{x coordinate(s)}
  \item{y}{y coordinate(s)}
  \item{xy}{matrix of x and y coordinates, or a SpatialPoints or SpatialPointsDataFrame object}
  \item{lns}{SpatialLines object}
  \item{p}{SpatialPolygons object}
  \item{weights}{Logical. If \code{TRUE}, the fraction of each cell that is covered is also returned}
  \item{duplicates}{Logical. If \code{TRUE}, the same cell number can be returned twice (if the point in the middle of a division between two cells) or four times (if a point is in the center of a cell)}
  \item{...}{additional arguments (none implemented)}
}
  
\details{
\code{cellFromRowCol} returns the cell numbers obtained for each row / col number pair. In contrast, \code{cellFromRowColCombine} returns the cell numbers obtained by the combination of all row and column numbers supplied as arguments.
  
  
\code{fourCellsFromXY} returns the four cells that are nearest to a point (if the point falls on the raster). Also see \code{\link{adjacent}}.
}

\value{
vector of row, column or cell numbers. \code{cellFromLine} and \code{cellFromPolygon} return a list, \code{fourCellsFromXY} returns a matrix. 
}

\seealso{
\code{\link{xyFromCell}, \link{cellsFromExtent}, \link{rowColFromCell}}
}



\examples{
r <- raster(ncols=10, nrows=10)
cellFromRowCol(r, 5, 5)
cellFromRowCol(r, 1:2, 1:2)
cellFromRowColCombine(r, 1:3, 1:2)
cellFromCol(r, 1)
cellFromRow(r, 1)

colFromX(r, 0.5)
rowFromY(r, 0.5)
cellFromXY(r, cbind(c(0.5,5), c(15, 88)))
fourCellsFromXY(r, cbind(c(0.5,5), c(15, 88)))

cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
pols <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), Polygons(list(Polygon(cds2)), 2)))
cellFromPolygon(r, pols)
}

\keyword{spatial}
