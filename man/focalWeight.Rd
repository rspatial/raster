\name{focalWeight}

\alias{focalWeight}

\title{Focal weights matrix}

\description{
Calculate focal ("moving window") weight matrix for use in the \code{\link{focal}} function. The sum of the values adds up to one.
}

\usage{
focalWeight(x, d, type=c('circle', 'Gauss', 'rectangle'))
}

\arguments{
\item{x}{Raster* object}
\item{d}{numeric. If \code{type=circle}, the radius of the circle (in units of the CRS). If \code{type=rectangle} the dimension of the rectangle (one or two numbers). If \code{type=Gauss} the size of sigma, and optionally another number to determine the size of the matrix returned (default is 3 times sigma)} 
\item{type}{character indicating the type of filter to be returned}
}

\value{
matrix that can be used in \code{\link{focal}}
}


\examples{
r <- raster(ncols=36, nrows=18, xmn=0)
# Gaussian filter for square cells
gf <- focalWeight(r, 2, "Gauss")
}


\keyword{spatial}

