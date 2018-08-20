\name{which}

\docType{methods}

\alias{Which}
\alias{Which,RasterLayer-method}

\title{Which cells are TRUE?}

\description{
\code{Which} returns a RasterLayer with \code{TRUE} or \code{FALSE} setting cells that are \code{NA} to \code{FALSE} (unless \code{na.rm=FALSE}). If the RasterLayer has numbers, all values that are 0 become \code{FALSE} and all other values become \code{TRUE}. The function can also return the cell numbers that are \code{TRUE}
}


\usage{
\S4method{Which}{RasterLayer}(x, cells=FALSE, na.rm=TRUE, ...)
}

\arguments{
  \item{x}{RasterLayer}
  \item{cells}{logical. If \code{TRUE}, cell numbers are returned, otherwise a RasterLayer is returned}
  \item{na.rm}{logical. If \code{TRUE}, \code{NA} values are treated as \code{FALSE}, otherwise they remain \code{NA} (only when \code{cells=FALSE})}
  \item{...}{Additional arguments (none implemented)} 
}


\seealso{ \code{\link{which.max}},  \code{\link{which.min}} }

\value{
RasterLayer
}



\examples{
r <- raster(ncol=10, nrow=10)
set.seed(0)
r[] <- runif(ncell(r))
r[r < 0.2 ] <- 0
r[r > 0.8] <- 1
r[r > 0 & r < 1 ] <- 0.5

Which(r, cells=TRUE)
Which(r > 0.5, cells=TRUE)

s1 <- r > 0.5
s2 <- Which(r > 0.5)
s1[1:15]
s2[1:15]

# this expression
x1 <- Which(r, na.rm=FALSE)
# is the inverse of
x2 <- r==0
}

\keyword{spatial}
