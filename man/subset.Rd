\name{subset}

\alias{subset}
\alias{subset,Raster-method}
\alias{subset,RasterStack-method}


\title{Subset layers in a Raster* object}

\description{
Extract a set of layers from a RasterStack or RasterBrick object.  
}

\usage{
\S4method{subset}{Raster}(x, subset, drop=TRUE, filename='', ...)

\S4method{subset}{RasterStack}(x, subset, drop=TRUE, filename='', ...)
}

\arguments{
  \item{x}{RasterBrick or RasterStack object}
  \item{subset}{integer or character. Should indicate the layers (represented as integer or by their name)}
  \item{drop}{If \code{TRUE}, a selection of a single layer will be returned as a RasterLayer}
  \item{filename}{character. Output filename (optional)}
  \item{...}{additional arguments as for \code{\link{writeRaster}}}
}

\value{
Raster* object
}



\seealso{ \code{\link[raster]{dropLayer}}}

\examples{
s <- stack(system.file("external/rlogo.grd", package="raster"))
sel <- subset(s, 2:3)

# Note that this is equivalent to
sel2 <- s[[2:3]]


# and in this particular case:
sel3 <- dropLayer(s, 1)

nlayers(s)
nlayers(sel)

# effect of 'drop=FALSE' when selecting a single layer
sel <- subset(s, 2)
class(sel)
sel <- subset(s, 2, drop=FALSE)
class(sel)
}

\keyword{ spatial }
