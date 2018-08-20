\name{localFun}

\docType{methods}

\alias{localFun}
\alias{localFun,RasterLayer,RasterLayer-method}


\title{Local functions}

\description{
Local functions for two RasterLayer objects (using a focal neighborhood)
}

\usage{
\S4method{localFun}{RasterLayer,RasterLayer}(x, y, ngb=5, fun, filename='', ...)
}

\arguments{
  \item{x}{RasterLayer or RasterStack/RasterBrick}
  \item{y}{object of the same class as \code{x}, and with the same number of layers}
  \item{ngb}{integer. rectangular neighbourhood size. Either a single integer or a vector of two integers c(rows, cols), such as c(3,3) to have a 3 x 3 focal window}
  \item{fun}{function}
  \item{filename}{character. Output filename (optional)}
  \item{...}{additional arguments as for \code{\link{writeRaster}}}
}

\note{The first two arguments that \code{fun} needs to accept are vectors representing the local cells of RasterLayer \code{x} and \code{y} (each of length \code{ngb * ngb}). It also must have an ellipsis (\code{...}) argument}

\value{
RasterLayer
}

\seealso{ \code{\link{corLocal}}, \code{\link{localFun}} }


\examples{
set.seed(0)
b <- stack(system.file("external/rlogo.grd", package="raster"))
x <- flip(b[[2]], 'y') + runif(ncell(b))
y <- b[[1]] + runif(ncell(b))

f <- localFun(x, y, fun=cor)

\dontrun{
# local regression:
rfun <- function(x, y, ...) {
	m <- lm(y~x)
	# return R^2
	summary(m)$r.squared
}

ff <- localFun(x, y, fun=rfun)
plot(f, ff)
}
}

\keyword{methods}
\keyword{spatial}

