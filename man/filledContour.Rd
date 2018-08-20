\name{filledContour}

\alias{filledContour}

\title{Filled contour plot}

\description{
Filled contour plot of a RasterLayer. This is a wrapper around \code{\link[graphics]{filled.contour}} for RasterLayer objects.
}

\usage{
filledContour(x, y=1, maxpixels=100000, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{y}{Integer. The layer number of x (if x has multiple layers)}
  \item{maxpixels}{The maximum number of pixels}
  \item{...}{Any argument that can be passed to \code{\link[graphics]{filled.contour}} (graphics package)}  
}

\seealso{ \code{\link[graphics]{filled.contour}}, \code{\link[raster]{persp}}, \code{\link[raster]{plot}} }



\examples{
r <- raster(system.file("external/test.grd", package="raster"))
filledContour(r)
}

\keyword{methods}
\keyword{spatial}

