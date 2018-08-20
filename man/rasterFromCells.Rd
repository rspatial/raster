\name{rasterFromCells}

\alias{rasterFromCells}

\title{Subset a raster by cell numbers}

\description{
This function returns a new raster based on an existing raster and cell numbers for that raster.
The new raster is cropped to the cell numbers provided, and, if \code{values=TRUE} has values that are the cell numbers of the original raster.
}

\usage{
rasterFromCells(x, cells, values=TRUE)
}

\arguments{
  \item{x}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{cells}{vector of cell numbers}
  \item{values}{Logical. If \code{TRUE}, the new RasterLayer has cell values that correspond to the cell numbers of \code{x}}
}
  
\details{
Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom.
The last cell number equals the number of cells of the Raster* object.
}

\value{
RasterLayer
}

\seealso{ \code{\link[raster]{rowFromCell}} }



\examples{
r <- raster(ncols=100, nrows=100)
cells <- c(3:5, 210)
r <- rasterFromCells(r, cells)
cbind(1:ncell(r), getValues(r))
}

\keyword{spatial}
