\name{animate}

\docType{methods}

\alias{animate}
\alias{animate,RasterStackBrick-method}

\title{Animate layers of a Raster* object}

\description{
Animate (sequentially plot) the layers of a RasterStack or RasterBrick* object to create a movie
}

\usage{
\S4method{animate}{RasterStackBrick}(x, pause=0.25, main, zlim, maxpixels=50000, n=10, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{pause}{numeric. How long should be the pause be between layers?}  
  \item{main}{title for each layer. If not supplied the z-value is used if available. Otherwise the names are used.}  
  \item{zlim}{numeric vector of lenght 2. Range of values to plot}
  \item{maxpixels}{integer > 0. Maximum number of cells to use for the plot. If \code{maxpixels < ncell(x)}, \code{sampleRegular} is used before plotting}
  \item{n}{integer > 0. Number of loops}
  \item{...}{Additional arguments passed to \code{\link{plot}}}
}


\value{
None
}

\seealso{ \code{\link{plot}}, \code{\link{spplot}}, \code{\link{plotRGB}} }

\examples{
b <- brick(system.file("external/rlogo.grd", package="raster"))
animate(b, n=1)
}

\keyword{methods}
\keyword{spatial}

