\name{disaggregate}

\alias{disaggregate}
\alias{disaggregate,Raster-method}

\title{Disaggregate}

\description{
Disaggregate a RasterLayer to create a new RasterLayer with a higher resolution (smaller cells). The values in the new RasterLayer are the same as in the larger original cells unless you specify \code{method="bilinear"}, in which case values are locally interpolated (using the \code{\link[raster]{resample}} function).
}

\usage{
\S4method{disaggregate}{Raster}(x, fact=NULL, method='', filename='', ...)
}

\arguments{
  \item{x}{a Raster object}
  \item{fact}{integer. amount of disaggregation expressed as number of cells (horizontally and vertically). This can be a single integer or two integers c(x,y), in which case the first one is the horizontal disaggregation factor and y the vertical disaggreation factor. If a single integer value is supplied, cells are disaggregated with the same factor in x and y direction}
  \item{method}{Character. \code{''} or \code{'bilinear'}. If \code{'bilinear'}, values are locally interpolated (using the \code{\link[raster]{resample}} function}
  \item{filename}{Character. Output filename (optional)}
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}
}


\value{
Raster object
}

\seealso{ \code{\link[raster]{aggregate}} }

\author{Robert J. Hijmans and Jim Regetz}

\examples{
r <- raster(ncols=10, nrows=10)
rd <- disaggregate(r, fact=c(10, 2))
ncol(rd)
nrow(rd)
values(r) <- 1:ncell(r)
rd <- disaggregate(r, fact=c(4, 2), method='bilinear')

}

\keyword{spatial}

