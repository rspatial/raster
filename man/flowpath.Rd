\name{flowPath}

\alias{flowPath}
  
\title{Flow path}

\description{
Compute the flow path (drainage path) starting at a given point. See package \code{gdistance} for more path computations.
}

\usage{
flowPath(x, p, ...)
}

\arguments{
\item{x}{RasterLayer of flow direction (as can be created with \code{\link{terrain}}}
\item{p}{starting point. Either two numbers: x (longitude) and y (latitude) coordinates; or a single cell number	}
\item{...}{additional arguments (none implemented)}  
}


\value{
numeric (cell numbers)
}

\author{Ashton Shortridge}

\examples{
data(volcano)
v <- raster(volcano, xmn=2667400, xmx=2668010, ymn=6478700, ymx=6479570, crs="+init=epsg:27200")
fd <- terrain(v, opt = "flowdir")
path <- flowPath(fd, 2407)
xy <- xyFromCell(fd, path)
plot(v)
lines(xy)
}

\keyword{spatial}
