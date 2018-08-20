\name{dim}

\alias{dim}
\alias{dim,RasterStackBrick-method}
\alias{dim,BasicRaster-method}
\alias{dim<-,BasicRaster-method}
\alias{dim<-,RasterLayer-method}
\alias{dim<-,RasterBrick-method}

\docType{methods}

\title{Dimensions of a Raster* object}

\description{
Get or set the number of rows, columns, and layers of a Raster* object. You cannot use this function to set the dimensions of a RasterStack object.

When setting the dimensions, you can provide a row number, or a vector with the row and the column number (for a RasterLayer and a RasterBrick), or a row and column number and the number of layers (only for a RasterBrick)
}

\usage{
\S4method{dim}{BasicRaster}(x)
}

\arguments{
 \item{x}{Raster(* object}
}

\value{
Integer or Raster* object
}
\seealso{ \code{ \link{ncell}, \link{extent}, \link{res} } } 



\examples{
r <- raster()
dim(r)
dim(r) <- c(18) 
dim(r)
dim(r) <- c(18, 36) 
dim(r)
b <- brick(r)
dim(b)
dim(b) <- c(10, 10, 5)
dim(b)
}

\keyword{spatial}

