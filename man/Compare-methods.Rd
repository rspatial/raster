\name{Compare-methods}

\docType{methods}

\alias{Compare-methods}

\alias{Compare,Extent,Extent-method}
\alias{Compare,Raster,Raster-method}
\alias{Compare,Raster,logical-method}
\alias{Compare,Raster,numeric-method}
\alias{Compare,logical,Raster-method}
\alias{Compare,numeric,Raster-method}

\alias{==,BasicRaster,BasicRaster-method}
\alias{!=,BasicRaster,BasicRaster-method}

\title{Compare Raster* objects}
\description{
These methods compare the location and resolution of Raster* objects. That is, they compare their spatial extent, projection, and number of rows and columns. 
 
For \code{BasicRaster} objects you can use \code{==} and \code{!=}, the values returned is a single logical value \code{TRUE} or \code{FALSE}
 
For RasterLayer objects, these operators also compare the values associated with the objects, and the result is a RasterLayer object with logical (Boolean) values. 
 
The following methods have been implemented for RasterLayer objects:
 
 \code{==, !=, >, <,  <=, >=}
}


\value{
A logical value or a RasterLayer object, and in some cases the side effect of a new file on disk.
}




\examples{
r1 <- raster()
r1 <- setValues(r1, round(10 * runif(ncell(r1))))
r2 <- setValues(r1, round(10 * runif(ncell(r1))))
as(r1, 'BasicRaster') == as(r2, 'BasicRaster')
r3 <- r1 == r2

b <- extent(0, 360, 0, 180)
r4 <- setExtent(r2, b)
as(r2, 'BasicRaster') != as(r4, 'BasicRaster')
# The following would give an error. You cannot compare RasterLayer 
# that do not have the same BasicRaster properties.
#r3 <- r1 > r4
}


\keyword{methods}
\keyword{math}
