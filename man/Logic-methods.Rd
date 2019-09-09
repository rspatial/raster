\name{Logic-methods}

\docType{methods}

\alias{Logic-methods}
\alias{Logic,Raster,Raster-method}
\alias{is.na,Raster-method}
\alias{is.nan,Raster-method}
\alias{is.finite,Raster-method}
\alias{is.infinite,Raster-method}
\alias{!,Raster-method}


\title{Logical operators and functions}
\description{
The following logical (boolean) operators are available for computations with RasterLayer objects: 

\code{&, |, and !}

The following functions are available with a Raster* argument: 

\code{is.na}, \code{is.nan}, \code{is.finite}, \code{is.infinite}
}

\value{
A Raster object with logical (\code{TRUE/FALSE} values)
}


\section{Note}{
 These are convenient operators/functions that are most usful for relatively small RasterLayers for which all the values can be held in memory. 
 If the values of the output RasterLayer cannot be held in memory, they will be saved to a temporary file. In that case it could be more 
 efficient to use \code{\link[raster]{calc}} instead.
}

\seealso{ \code{\link[raster]{Math-methods}}, \code{\link[raster]{overlay}}, \code{\link[raster]{calc}} }




\examples{
r <- raster(ncols=10, nrows=10)
values(r) <- runif(ncell(r)) * 10
r1 <- r < 3 | r > 6
r2 <- !r1
r3 <- r >= 3 & r <= 6
r4 <- r2 == r3
r[r>3] <- NA
r5 <- is.na(r)
r[1:5]
r1[1:5]
r2[1:5]
r3[1:5]

}

\keyword{methods}
\keyword{math}
