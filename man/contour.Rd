\name{contour}

\docType{methods}

\alias{contour}
\alias{contour,RasterLayer-method}

\title{Contour plot}

\description{
Contour plot of a RasterLayer. 
}

\usage{
\S4method{contour}{RasterLayer}(x, maxpixels=100000, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{maxpixels}{maximum number of pixels used to create the contours}
  \item{...}{any argument that can be passed to \code{\link[graphics]{contour}} (graphics package)}  
}


\seealso{ 
\code{\link{persp}}, \code{\link{filledContour}}, \code{\link{rasterToContour} }
}

\examples{
r <- raster(system.file("external/test.grd", package="raster"))
plot(r)
contour(r, add=TRUE)
}

\keyword{methods}
\keyword{spatial}

