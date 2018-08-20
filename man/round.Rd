\name{round}

\docType{methods}

\alias{round,RasterLayer-method}
\alias{trunc,RasterLayer-method}
\alias{ceiling,RasterLayer-method}
\alias{floor,RasterLayer-method}

\title{Integer values}

\description{
These functions take a single RasterLayer argument \code{x} and change its values to integers.

\code{ceiling} returns a RasterLayer with the smallest integers not less than the corresponding values of x. 

\code{floor} returns a RasterLayer with the largest integers not greater than the corresponding values of x. 

\code{trunc} returns a RasterLayer with the integers formed by truncating the values in x toward 0. 

\code{round} returns a RasterLayer with values rounded to the specified number of digits (decimal places; default 0). 
}


\section{Methods}{
\describe{
ceiling(x)
floor(x)
trunc(x, ...)
round(x, digits = 0)


\item{x}{a RasterLayer object}
\item{digits}{integer indicating the precision to be used}
\item{...}{additional arguments}
}
}


\details{
see ?base::round
}

\seealso{ \code{\link[base]{round}} }

\value{
a RasterLayer object
}


\examples{
r <- raster(ncol=10, nrow=10)
r[] <- runif(ncell(r)) * 10
s <- round(r)

}
\keyword{spatial}
