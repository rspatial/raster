\name{setExtent}

\alias{setExtent}
\alias{extent<-}
  
\title{Set the extent of a RasterLayer}

\description{
setExtent sets the extent of a Raster* object. Either by providing a new Extent object or by setting the extreme
coordinates one by one.
}

\usage{
setExtent(x, ext, keepres=FALSE, snap=FALSE)
extent(x) <- value
}

\arguments{
  \item{x}{A Raster* object}
  \item{ext}{ An object of class Extent (which you can create with \code{\link[raster]{extent}}, or an object that has an extent (e.g. a Raster* or Spatial* object) ) }   
  \item{keepres}{logical. If \code{TRUE}, the resolution of the cells will stay the same after adjusting the bounding box (by adjusting the number of rows and columns). 
  If \code{FALSE}, the number of rows and columns will stay the same, and the resolution will be adjusted.
}
  \item{snap}{logical. If \code{TRUE}, the extent is adjusted so that the cells of the input and output RasterLayer are aligned}
  \item{value}{An object of class Extent (which you can create with \code{\link[raster]{extent}} )}  
}


\value{
a Raster* object
}
  


\seealso{  \code{\link[raster]{extent}}, \code{\link[raster]{Extent-class}} }

\examples{
r <- raster()
bb <- extent(-10, 10, -20, 20)
extent(r) <- bb
r <- setExtent(r, bb, keepres=TRUE)

}

\keyword{spatial}
