\name{sampleStratified}

\alias{sampleStratified}
\alias{sampleStratified,RasterLayer-method}


\title{Stratified random sample}

\description{
Take a stratified random sample from the cell values of a Raster* object (without replacement). An attempt is made to sample \code{size} cells from each stratum. The values in the RasterLayer \code{x} are rounded to integers; with each value representing a stratum.
}

\usage{
\S4method{sampleStratified}{RasterLayer}(x, size, exp=10, na.rm=TRUE, xy=FALSE, ext=NULL, sp=FALSE, ...)
}


\arguments{
  \item{x}{Raster* object, with values (rounded to integers) representing strata}
  \item{size}{positive integer giving the number of items to choose} 
  \item{exp}{numeric >= 1. 'Expansion factor' that is multiplied with size to get an intial sample. Can be increased when you get an insufficient number of samples for small strata}
  \item{na.rm}{logical. If \code{TRUE} (the default), \code{NA} values are removed from random sample}
  \item{xy}{logical. Return coordinates of cells rather than cell numbers}
  \item{ext}{Extent object. To limit regular sampling to the area within the extent}
  \item{sp}{logical. If \code{TRUE}, a SpatialPointsDataFrame is returned}  
  \item{...}{Additional arguments. None implemented} 
}

\details{
The function may not work well when the size (number of cells) of some strata is relatively small.
}

\value{
matrix of cell numbers (and optionally coordinates) by stratum
}

\seealso{\code{\link{sampleRandom}, \link{sampleRegular}}}



\examples{
 r <- raster(ncol=10, nrow=10)
 names(r) <- 'stratum'
 values(r) <- round((runif(ncell(r))+0.5)*3)
 sampleStratified(r, size=3)
}

\keyword{spatial}
