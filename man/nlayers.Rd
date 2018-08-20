\name{nlayers}

\alias{nlayers}
\alias{nlayers,BasicRaster-method}
\alias{nlayers,Raster-method}
\alias{nlayers,RasterStack-method}
\alias{nlayers,RasterBrick-method}
\alias{nlayers,Spatial-method}
  
\title{Number of layers}

\description{
Get the number of layers in a Raster* object, typically used with a (multilayer) RasterStack or RasterBrick object
}

\usage{
nlayers(x)
}

\arguments{
\item{x}{Raster* object}
}

\value{
integer
}

\seealso{ \code{\link[raster]{names}} }



\examples{
r <- raster(ncols=10, nrows=10)
r[] <- 1:ncell(r)
s <- stack(r, r, r)
nlayers(s)
s <- stack(s,s)
nlayers(s)
s <- dropLayer(s, 2:3)
nlayers(s)

}

\keyword{spatial}
