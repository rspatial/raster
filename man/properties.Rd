\name{properties}

\alias{dataSize} 
\alias{dataSigned} 

  
\title{Raster file properties}

\description{
  Properties of the values of the file that a RasterLayer object points to
  
  \code{dataSize} returns the number of bytes used for each value (pixel, grid cell)
  \code{dataSigned} is TRUE for data types that include negative numbers.
  
}

\usage{
dataSize(object)
dataSigned(object)
}

\arguments{
  \item{object}{Raster* object}
}

\seealso{ \code{\link[raster]{filename}} }


\value{
  varies
}



\examples{
r <- raster(system.file("external/test.grd", package="raster"))
dataSize(r)
dataSigned(r)
dataType(r)
}

\keyword{spatial}
