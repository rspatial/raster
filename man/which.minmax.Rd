\name{which.min}

\docType{methods}

\alias{which.min}
\alias{which.max}
\alias{whiches.min}
\alias{whiches.max}
\alias{which.min,RasterLayer-method}
\alias{which.max,RasterLayer-method}
\alias{which.min,RasterStackBrick-method}
\alias{which.max,RasterStackBrick-method}
\alias{whiches.min,RasterStackBrick-method}
\alias{whiches.max,RasterStackBrick-method}

\title{Where is the min or max value?}

\description{Which cells have the minumum / maximum value (for a RasterLayer), or which layer has the minimum/maximum value (for a RasterStack or RasterBrick)?

which.min and which.max return the index of the first layer that has the min or max value for a cell. This can be problematic if there are ties. 

In you want the index of all the layers that have the min or max value, use whiches.min or whiches.max (only for objects with less than 10 layers).
}

\usage{
which.min(x)
which.max(x)
whiches.min(x, ...)
whiches.max(x, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{...}{additional arguments (none implemented)}
}

\value{
(which.*): vector of cell numbers (if \code{x} is a RasterLayer). If \code{x} is a RasterStack or RasterBrick, a RasterLayer giving the number of the first layer with the minimum or maximum value for a cell.

(whiches.*). An integer in which each digit represents a layer. For example, 35 means "layers 3 and 5"
}

\note{
There is a limit to accurate integer number representation. Therefore, do not use \code{whiches.*} with more than 15 layers.
}

\seealso{ \code{\link{Which}}}


\examples{
b <- brick(system.file("external/rlogo.grd", package="raster")) 

r <- which.min(b)

i <- which.min(b[[3]])
xy <- xyFromCell(b, i)
plot(b[[3]])
points(xy)

x <- whiches.min(b)
freq(x)

}

\keyword{spatial}
