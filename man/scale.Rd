\name{scale}

\alias{scale}
\alias{scale,Raster-method}


\title{Scale values}

\description{
Center and/or scale raster data 
}

\usage{
\S4method{scale}{Raster}(x, center=TRUE, scale=TRUE)
}


\arguments{
 \item{x}{Raster* object}
 \item{center}{logical or numeric. If \code{TRUE}, centering is done by subtracting the layer means (omitting \code{NA}s), and if \code{FALSE}, no centering is done. If \code{center} is a numeric vector with length equal to the \code{nlayers(x)}, then each layer of \code{x} has the corresponding value from center subtracted from it.}
  \item{scale}{logical or numeric. If \code{TRUE}, scaling is done by dividing the (centered) layers of \code{x} by their standard deviations if \code{center} is \code{TRUE}, and the root mean square otherwise. If scale is \code{FALSE}, no scaling is done. If \code{scale} is a numeric vector with length equal to \code{nlayers(x)}, each layer of \code{x} is divided by the corresponding value. Scaling is done after centering.}
}
 
\value{
Raster* object
}


\seealso{ \code{\link[base]{scale}} }

\examples{
b <- brick(system.file("external/rlogo.grd", package="raster"))
bs <- scale(b)
}

\keyword{ spatial }
