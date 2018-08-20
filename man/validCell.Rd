\name{validCell}

\alias{validCell}
\alias{validCol}
\alias{validRow}

  
\title{Validity of a cell, column or row number}

\description{
Simple helper functions to determine if a row, column or cell number is valid for a certain Raster* object
}

\usage{
validCell(object, cell) 
validCol(object, colnr) 
validRow(object, rownr) 
}

\arguments{
  \item{object}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{cell}{cell number(s)}
  \item{colnr}{column number; or vector of column numbers}
  \item{rownr}{row number; or vector of row numbers}
}
  
\value{
  logical value
}



\examples{
#using a new default raster (1 degree global)
r <- raster()
validCell(r, c(-1, 0, 1))
validRow(r, c(-1, 1, 100, 10000))
}

\keyword{spatial}
