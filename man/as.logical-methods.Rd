\name{as.logical}

\docType{methods}

\alias{as.logical,Raster-method}
\alias{as.integer,Raster-method}

\title{ Change cell values to logical or integer values}

\description{
 Change values of a Raster* object to logical or integer values. With \code{as.logical}, zero becomes \code{FALSE}, all other values become \code{TRUE}. With \code{as.integer} values are truncated.
}

\usage{
\S4method{as.logical}{Raster}(x, filename='', ...)

\S4method{as.integer}{Raster}(x, filename='', ...)
}

\arguments{
\item{x}{Raster* object}
\item{filename}{character. Output filename (optional)}
\item{...}{additional optional arguments as for \code{\link{writeRaster}}}
}

\seealso{ \code{\link[base]{logical}}, \code{\link[base]{integer}}  }

\examples{
r <- raster(nrow=10, ncol=10)
set.seed(0)
values(r) <- runif(ncell(r)) * 10
r
r <- as.integer(r)
r
as.logical(r)
}


\keyword{methods}
\keyword{spatial}
