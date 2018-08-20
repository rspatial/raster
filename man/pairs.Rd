\name{pairs}

\docType{methods}

\alias{pairs}
\alias{pairs,RasterStackBrick-method}

\title{
Pairs plot (matrix of scatterplots)
}

\description{
Pair plots of layers in a RasterStack or RasterBrick. This is a wrapper around graphics function \code{\link[graphics]{pairs}}.
}

\usage{
\S4method{pairs}{RasterStackBrick}(x, hist=TRUE, cor=TRUE, use="pairwise.complete.obs", maxpixels=100000, ...)
}

\arguments{
\item{x}{RasterBrick or RasterStack}
\item{hist}{Logical. If TRUE a histogram of the values is shown on the diagonal}
\item{cor}{Logical. If TRUE the correlation coefficient is shown in the upper panels}
\item{use}{Argument passed to the \code{\link[stats]{cor}} function}
\item{maxpixels}{Integer. Number of pixels to sample from each layer of large Raster objects}
\item{...}{Additional arguments (only \code{cex} and \code{main})}
}

\seealso{
\code{\link{boxplot}, \link{hist}, \link{density}}
}

\examples{
r <- raster(system.file("external/test.grd", package="raster") )
s <- stack(r, 1/r, sqrt(r))
pairs(s)

\dontrun{
# to make indvidual histograms:
hist(r)
# or scatter plots:
plot(r, 1/r)
}
}



\keyword{spatial}

