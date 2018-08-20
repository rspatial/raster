\name{resolution}

\alias{xres}
\alias{yres}
\alias{res}
\alias{xres,BasicRaster-method}
\alias{yres,BasicRaster-method}
\alias{res,BasicRaster-method}


\alias{res<-}


\title{Resolution}

\description{
Get (or set) the x and/or y resolution of a Raster* object
}

\usage{
xres(x)
yres(x)
res(x)
res(x) <- value
}

\arguments{
  \item{x}{Raster* object}
  \item{value}{Resolution (single number or vector of two numbers) }  
}


\value{
A single numeric value or two numeric values.
}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{ncell}}  }



\examples{
r <- raster(ncol=18, nrow=18)
xres(r)
yres(r)
res(r)

res(r) <- 1/120
# set yres differently
res(r) <- c(1/120, 1/60)


}

\keyword{spatial}
