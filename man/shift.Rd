\name{shift}

\docType{methods}

\alias{shift}
\alias{shift,Raster-method}
\alias{shift,SpatialPolygons-method}
\alias{shift,SpatialLines-method}
\alias{shift,SpatialPoints-method}
  
\title{Shift}

\description{
Shift the location of a Raster* of vector type Spatial* object in the x and/or y direction
}

\usage{
\S4method{shift}{Raster}(x, dx=0, dy=0, filename='', ...)

\S4method{shift}{SpatialPolygons}(x, dx=0, dy=0,  ...)

\S4method{shift}{SpatialLines}(x, dx=0, dy=0,  ...)

\S4method{shift}{SpatialPoints}(x, dx=0, dy=0,  ...)
}

\arguments{
  \item{x}{Raster* or Spatial* object}
  \item{dx}{numeric. The shift in horizontal direction}
  \item{dy}{numeric. The shift in vertical direction}
  \item{filename}{character file name (optional)}
  \item{...}{if \code{x} is a Raster* object: additional arguments as for \code{\link{writeRaster}} } 
}
 
\value{
Same object type as \code{x}
}


\seealso{ \code{\link{flip}}, \code{\link{rotate}}, and the elide function in the maptools package }

\examples{
r <- raster()
r <- shift(r, x=1, y=-1)
}

\keyword{spatial}
