\name{as.matrix}

\alias{as.vector}
\alias{as.matrix}
\alias{as.array}
\alias{as.array,RasterStackBrick-method}
\alias{as.array,RasterLayer-method}
\alias{as.matrix,RasterStackBrick-method}
\alias{as.matrix,RasterLayer-method}
\alias{as.matrix,Extent-method}
\alias{as.vector,Extent-method}
\alias{as.vector,Raster-method}
  
 
\title{Get a vector, matrix, or array with raster cell values}

\description{
\code{as.vector} returns a vector of cell values. For a RasterLayer it is equivalent to getValues(x). 

\code{as.matrix} returns all values of a Raster* object as a matrix. For RasterLayers, rows and columns in the matrix represent rows and columns in the RasterLayer object. For other Raster* objects, the matrix returned by \code{as.matrix} has columns for each layer and rows for each cell. 

\code{as.array} returns an array of matrices that are like those returned by \code{as.matrix} for a RasterLayer

If there is insufficient memory to load all values, you can use \code{\link{getValues}} or \code{\link{getValuesBlock}} to read chunks of the file.

\code{as.matrix} and \code{as.vector} can also be used to obtain the coordinates from an Extent object.
}

\usage{
as.matrix(x, ...)
as.array(x, ...)

\S4method{as.vector}{Extent}(x, mode='any')

\S4method{as.vector}{Raster}(x, mode='any')
}

\arguments{
 \item{x}{ Raster* or (for \code{as.matrix} and \code{as.vector}) Extent object }
 \item{mode}{Character string giving an atomic mode (such as "numeric" or "character") or "list", or "any". Note: this argument is currently ignored!}
 \item{...}{ additional arguments: 
 
	\code{maxpixels} Integer. To regularly subsample very large objects 
	
    \code{transpose} Logical. Transpose the data? (for as.array only)

}}
  

\value{
matrix, array, or vector
}



\examples{
r <- raster(ncol=3, nrow=3)
values(r) <- 1:ncell(r)
as.matrix(r)
s <- stack(r,r)
as.array(s)
as.vector(extent(s))
}

\keyword{spatial}
\keyword{methods}
