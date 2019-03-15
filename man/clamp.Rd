\name{clamp}

\alias{clamp}
\alias{clamp,Raster-method}
\alias{clamp,numeric-method}

\title{Clamp values}

\description{
Clamp values to a minimum and maximum value. That is, all values below the lower clamp value and above the upper clamp value become NA (or the lower/upper value if \code{useValue=TRUE}) 
}

\usage{
\S4method{clamp}{Raster}(x, lower=-Inf, upper=Inf, useValues=TRUE, filename="", ...)
\S4method{clamp}{numeric}(x, lower=-Inf, upper=Inf, ...)
}

\arguments{
  \item{x}{RasterLayer, or numeric vector}
  \item{lower}{numeric. lowest value}
  \item{upper}{numeric. highest value}
  \item{useValues}{logical. If \code{FALSE} values outside the clamping range become \code{NA}, if \code{TRUE}, they get the extreme values}
  \item{filename}{character. Filename for the output RasterLayer (optional)}  
  \item{...}{additional arguments as for \code{\link{writeRaster}}}  
}

\value{
Raster object
}

\seealso{
\code{\link{reclassify}}
}

\examples{
r <- raster(ncols=12, nrows=12)
values(r) <- 1:ncell(r)
rc <- clamp(r, 25, 75) 
rc
}

\keyword{spatial}

