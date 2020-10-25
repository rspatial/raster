\name{extent}

\alias{extent}
\alias{extent,Extent-method}
\alias{extent,BasicRaster-method}
\alias{extent,Spatial-method}
\alias{extent,sf-method}
\alias{extent,bbox-method}
\alias{extent,matrix-method}
\alias{extent,numeric-method}
\alias{extent,list-method}
\alias{extent,GridTopology-method}
\alias{bbox,Raster-method}
\alias{bbox,Extent-method}


  
\title{Extent}

\description{
This function returns an Extent object of a Raster* or Spatial* object (or an Extent object), or creates an 
Extent object from a 2x2 matrix (first row: xmin, xmax; second row: ymin, ymax), vector (length=4; order= xmin, xmax, ymin, ymax) or list (with at least two elements, with names 'x' and 'y')

\code{bbox} returns a \code{sp} package like 'bbox' object (a matrix)
}

\usage{
extent(x, ...)
}

\arguments{
\item{x}{Raster* or Extent object, a matrix, a bbox, or a vector of four numbers } 
\item{...}{Additional arguments. When x is a single number representing 'xmin', you can pass three additional numbers (xmax, ymin, ymax)

When \code{x} is a Raster* object, you can pass four additional arguments to crop the extent: \code{r1, r2, c1, c2}, representing the first and last row and column number }
}
 

 
\value{
Extent object
}
  
\author{Robert J. Hijmans; Etienne Racine wrote the extent function for a list}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{drawExtent}} }

\examples{
r <- raster()
extent(r)
extent(c(0, 20, 0, 20))
#is equivalent to
extent(0, 20, 0, 20)
extent(matrix(c(0, 0, 20, 20), nrow=2))
x <- list(x=c(0,1,2), y=c(-3,5))
extent(x)

#crop the extent by row and column numbers
extent(r, 1, 20, 10, 30)
}

\keyword{spatial}
