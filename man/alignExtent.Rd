\name{alignExtent}

\alias{alignExtent}
  
\title{Align an extent (object of class Extent)}

\description{
Align an Extent object with the (boundaries of the) cells of a Raster* object
}

\usage{
alignExtent(extent, object, snap='near')
}

\arguments{
  \item{extent}{Extent object}  
  \item{object}{Raster* object}
  \item{snap}{Character. One of 'near', 'in', or 'out', to determine in which direction the extent should be aligned. To the nearest border, inwards or outwards}  
}
 
\value{
Extent object
}
 
\details{
Aligning an Extent object to another object assures that it gets the same origin and resolution. This should only be used to adjust objects because of imprecision in the data. alignExtent should not be used to force data to match that really does not match (use e.g. \code{\link{resample}} or (dis)aggregate for this).
}
  
  


\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{drawExtent}}, \code{\link[raster]{Extent-class}} }

\examples{
r <- raster()
e <- extent(-10.1, 9.9, -20.1, 19.9)
ea <- alignExtent(e, r)
e
extent(r)
ea

}

\keyword{spatial}
