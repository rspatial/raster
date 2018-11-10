\name{Arith-methods}

\docType{methods}

\alias{Arith-methods}
\alias{Arith,Extent,numeric-method}
\alias{Arith,Raster,Raster-method}
\alias{Arith,Raster,missing-method}
\alias{Arith,RasterLayer,logical-method}
\alias{Arith,RasterLayer,numeric-method}
\alias{Arith,RasterLayerSparse,numeric-method}
\alias{Arith,RasterStackBrick,logical-method}
\alias{Arith,RasterStackBrick,numeric-method}
\alias{Arith,logical,RasterLayer-method}
\alias{Arith,logical,RasterStackBrick-method}
\alias{Arith,numeric,Extent-method}
\alias{Arith,numeric,RasterLayer-method}
\alias{Arith,numeric,RasterLayerSparse-method}
\alias{Arith,numeric,RasterStackBrick-method}

\alias{+,SpatialPolygons,SpatialPolygons-method}
\alias{+,SpatialLines,SpatialLines-method}
\alias{+,SpatialPoints,SpatialPoints-method}
\alias{-,SpatialPolygons,SpatialPolygons-method}
\alias{*,SpatialPolygons,SpatialPolygons-method}

\title{Arithmetic with Raster* objects}

\description{
Standard arithmetic operators for computations with Raster* objects and numeric values. The following operators are available:
\code{ +, -, *, /, ^, \%\%, \%/\% }

The input Raster* objects should have the same extent, origin and resolution. If only the extent differs, the computation will continue for 
the intersection of the Raster objects. Operators are applied on a cell by cell basis.
For a RasterLayer, numeric values are recycled by row.  For a RasterStack or RasterBrick, recycling is done by layer. 
RasterLayer objects can be combined RasterStack/Brick objects, in which case the RasterLayer is 'recycled'.
When using multiple RasterStack or RasterBrick objects, the number of layers of these objects needs to be the same. 

In addition to arithmetic with Raster* objects, the following operations are supported for SpatialPolygons* objects.
Given SpatialPolygon objects \code{x} and \code{y}:

\code{x+y} is the same as \code{\link{union}(x, y)}. For SpatialLines* and SpatialPoints* it is equivalent to \code{\link{bind}(x, y)}

\code{x*y} is the same as \code{\link{intersect}(x, y)}

\code{x-y} is the same as \code{\link{erase}(x, y)}
}

\details{
If the values of the output Raster* cannot be held in memory, they will be saved to a temporary file. 
You can use \code{\link{options}} to set the default file format, datatype and progress bar.
}


\value{
A Raster* object, and in some cases the side effect of a new file on disk.
}

\seealso{ \code{\link[raster]{Math-methods}}, \code{\link[raster]{overlay}}, \code{\link[raster]{calc}} }



\examples{
r1 <- raster(ncols=10, nrows=10)
r1[] <- runif(ncell(r1))
r2 <- setValues(r1, 1:ncell(r1) / ncell(r1) )
r3 <- r1 + r2
r2 <- r1 / 10
r3 <- r1 * (r2 - 1 + r1^2 / r2)

# recycling by row
r4 <- r1 * 0 + 1:ncol(r1)

# multi-layer object mutiplication, no recycling
b1 <- brick(r1, r2, r3)
b2 <- b1 * 10

# recycling by layer
b3 <- b1 + c(1, 5, 10)

# addition of the cell-values of two RasterBrick objects
b3 <- b2 + b1

# summing two RasterBricks and one RasterLayer. The RasterLayer is 'recycled'
b3 <- b1 + b2 + r1
}

\keyword{methods}
\keyword{math}
\keyword{spatial}
