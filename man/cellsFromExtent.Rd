\name{cellsFromExtent}

\alias{cellsFromExtent}
\alias{extentFromCells}

\title{Cells from extent, and vice versa}

\description{
cellsFromExtent returns the cell numbers for a Raster* object that are within a specfied extent (rectangular area), supply an object of class Extent, or another Raster* object.

extentFromCells returns an Extent object from a Raster* object and cell numbers. All cells are within the returned Extent. 
}

\usage{
cellsFromExtent(object, extent, expand=FALSE)
extentFromCells(object, cells)
}

\arguments{
  \item{object}{A Raster* object}
  \item{extent}{An object of class Extent (which you can create with newExtent(), or another Raster* object )}  
  \item{expand}{Logical. If \code{TRUE}, \code{NA} is returned for (virtual) cells implied by \code{bndbox}, 
      that are outside the RasterLayer (\code{object}). If \code{FALSE}, only cell numbers for the area where \code{object} and \code{bndbox}
      overlap are returned (see \link[raster]{intersect}) }
  \item{cells}{numeric. A vector of cell numbers}
}

\value{
a vector of cell numbers 
}  
  


\seealso{ 
 \code{\link[raster]{extent}}, \code{\link{cellFromXY}}
}

\examples{
r <- raster()
bb <- extent(-5, 5, -5, 5)
cells <- cellsFromExtent(r, bb)
r <- crop(r, bb)
r[] <- cells

e <- extentFromCells(r, 50:55)
}

\keyword{spatial}
