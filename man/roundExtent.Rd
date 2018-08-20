\name{Extent math}

\alias{floor,Extent-method}
\alias{ceiling,Extent-method}
  
\title{round Extent coordinates}

\description{

use \code{round(x, digits=0)} to round the coordinates of an Extent object to the number of digits specified. This can be useful when dealing with a small imprecision in the data (e.g. 179.9999 instead of 180). \code{floor} and \code{ceiling} move the coordiantes to the outer or inner whole integer numbers.

It is also possible to use Arithmetic functions with Extent objects (but these work perhaps unexpectedly!)

See \code{\link[raster]{Math-methods}} for these (and many more) methods with Raster* objects.
}

\usage{
\S4method{floor}{Extent}(x)
\S4method{ceiling}{Extent}(x)
}

\arguments{
\item{x}{Extent object }
}


\seealso{\code{\link[raster]{Math-methods}}}
  

\examples{
e <- extent(c(0.999999,  10.000011, -60.4, 60))
round(e)
ceiling(e)
floor(e)
}

\keyword{ spatial }


