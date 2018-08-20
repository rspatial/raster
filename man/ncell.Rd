\name{ncell}

\alias{ncol}
\alias{nrow}
\alias{nrow,BasicRaster-method}
\alias{ncol,BasicRaster-method}
\alias{ncell}
\alias{ncell,ANY-method}
\alias{ncell,BasicRaster-method}
\alias{length,BasicRaster-method}
\alias{nrow<-}
\alias{ncol<-}


\title{Number or rows, columns, and cells of a Raster* object}

\description{
Get the number of rows, columns, or cells of a Raster* object.
}

\usage{
ncol(x)
nrow(x)
ncell(x)
ncol(x) <- value
nrow(x) <- value
}


\arguments{
\item{x}{a Raster object}
\item{value}{row or column number (integer > 0)}  
}

\value{
Integer
}

\seealso{ \code{\link{dim}, \link{extent}}, \link{res} }





\examples{
r <- raster()
ncell(r)
ncol(r)
nrow(r)
dim(r)

nrow(r) <- 18
ncol(r) <- 36
# equivalent to
dim(r) <- c(18, 36) 
}

\keyword{spatial}

