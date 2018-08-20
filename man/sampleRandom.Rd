\name{sampleRandom}

\alias{sampleRandom}
\alias{sampleRandom,Raster-method}


\title{Random sample}

\description{
Take a random sample from the cell values of a Raster* object (without replacement). 
}

\usage{
\S4method{sampleRandom}{Raster}(x, size, na.rm=TRUE, ext=NULL, 
    cells=FALSE, rowcol=FALSE, xy=FALSE, sp=FALSE, asRaster=FALSE, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{size}{positive integer giving the number of items to choose} 
  \item{na.rm}{logical. If \code{TRUE} (the default), \code{NA} values are removed from random sample}
  \item{ext}{Extent object. To limit regular sampling to the area within the extent}
  \item{cells}{logical. If \code{TRUE}, sampled cell numbers are also returned}
  \item{rowcol}{logical. If \code{TRUE}, sampled row and column numbers are also returned}
  \item{xy}{logical. If \code{TRUE}, coordinates of sampled cells are also returned}
  \item{sp}{logical. If \code{TRUE}, a SpatialPointsDataFrame is returned}
  \item{asRaster}{logical. If \code{TRUE}, a Raster* object is returned with random cells with values, all other cells with \code{NA}}
  \item{...}{Additional arguments as in \code{\link{writeRaster}}. Only relevant when \code{asRaster=TRUE}} 
}

\details{
With argument \code{na.rm=TRUE}, the returned sample may be smaller than requested
}

\value{
A vector, matrix (if \code{cells=TRUE} or \code{x} is a multi-layered object), or a SpatialPointsDataFrame (if \code{sp=TRUE} )
}

\seealso{\code{\link{sampleRegular}, \link{sampleStratified}}}



\examples{
 r <- raster(system.file("external/test.grd", package="raster"))
 sampleRandom(r, size=10)
 s <- stack(r, r)
 sampleRandom(s, size=5, cells=TRUE, sp=TRUE)
}

\keyword{spatial}
