\name{boxplot}

\docType{methods}

\alias{boxplot}
\alias{boxplot,RasterLayer-method}
\alias{boxplot,RasterStackBrick-method}

\title{
Box plot of Raster objects
}

\description{
Box plot of layers in a Raster object
}

\usage{
\S4method{boxplot}{RasterStackBrick}(x, maxpixels=100000, ...)

\S4method{boxplot}{RasterLayer}(x, y=NULL, maxpixels=100000, ...)
}

\arguments{
\item{x}{Raster* object}
\item{y}{If \code{x} is a RasterLayer object, y can be an additional RasterLayer to group the values of \code{x} by 'zone'}
\item{maxpixels}{Integer. Number of pixels to sample from each layer of large Raster objects}
\item{...}{Arguments passed to \code{graphics::\link[graphics]{boxplot}}}
}

\seealso{
\code{\link{pairs}, \link{hist}}
}

\examples{
r1 <- r2 <- r3 <- raster(ncol=10, nrow=10)
r1[] <- rnorm(ncell(r1), 100, 40)
r2[] <- rnorm(ncell(r1), 80, 10)
r3[] <- rnorm(ncell(r1), 120, 30)
s <- stack(r1, r2, r3)
names(s) <- c('A', 'B', 'C')

boxplot(s, notch=TRUE, col=c('red', 'blue', 'orange'), main='Box plot', ylab='random' )
}


\keyword{spatial}

