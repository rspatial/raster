\name{crosstab}

\docType{methods}

\alias{crosstab}
\alias{crosstab,Raster,Raster-method}
\alias{crosstab,RasterStackBrick,missing-method}

\title{Cross-tabulate}

\description{
Cross-tabulate two RasterLayer objects, or mulitiple layers in a RasterStack or RasterBrick to create a contingency table.
}

\usage{
\S4method{crosstab}{Raster,Raster}(x, y, digits=0, long=FALSE, useNA=FALSE, progress='', ...)

\S4method{crosstab}{RasterStackBrick,missing}(x, digits=0, long=FALSE, useNA=FALSE, progress='', ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{y}{Raster* object if \code{x} is a RasterLayer; Can be missing if \code{x} is a RasterStack or RasterBrick}
  \item{digits}{integer. The number of digits for rounding the values before cross-tabulation}
  \item{long}{logical. If \code{TRUE} the results are returned in 'long' format data.frame instead of a table}
  \item{useNA}{logical, indicting if the table should includes counts of \code{NA} values}
  \item{progress}{character. "text", "window", or "" (the default, no progress bar), only for large files that cannot be processed in one step}
  \item{...}{additional arguments. none implemented}
}


\value{
A table or data.frame
}

\seealso{ \code{\link[raster]{freq}}, \code{\link[raster]{zonal}} }



\examples{
r <- raster(nc=5, nr=5)
values(r) <- runif(ncell(r)) * 2
s <- setValues(r, runif(ncell(r)) * 3)
crosstab(r,s)

rs <- r/s
r[1:5] <- NA
s[20:25] <- NA
x <- stack(r, s, rs)
crosstab(x, useNA=TRUE, long=TRUE)
}

\keyword{methods}
\keyword{spatial}

