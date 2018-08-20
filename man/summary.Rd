\name{Summary}

\docType{methods}

\alias{summary,RasterLayer-method}
\alias{summary,RasterStackBrick-method}

\title{Summary}

\description{
Summarize a Raster* object. A sample is used for very large files. 
}

\usage{
\S4method{summary}{RasterLayer}(object, maxsamp=100000, ...)
}

\arguments{
  \item{object}{Raster* object}
  \item{maxsamp}{positive integer. Sample size used for large datasets}
  \item{...}{additional arguments. None implemented}
}


\value{matrix with (an estimate of) the median, minimum and maximum values, the first and third quartiles, and the number of cells with \code{NA} values}


\seealso{ \code{\link{cellStats}, link[raster]{quantile}}  }


\keyword{methods}
\keyword{spatial}
