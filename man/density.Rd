\name{density}

\alias{density}
\alias{density,Raster-method}

\docType{methods}

\title{Density plot}

\description{
Create density plots of values in a Raster object
}

\usage{
\S4method{density}{Raster}(x, layer, maxpixels=100000, plot=TRUE, main, ...)
}

\arguments{
\item{x}{Raster object}
\item{layer}{numeric. Can be used to subset the layers to plot in a multilayer object (RasterBrick or RasterStack)}
\item{maxpixels}{the maximum number of (randomly sampled) cells to be used for creating the plot}
\item{plot}{if \code{TRUE} produce a plot, else return a density object}
\item{main}{main title for each plot (can be missing)}
\item{...}{Additional arguments passed to \code{\link[base]{plot}}}
}

\value{
density plot (and a density object, returned invisibly if \code{plot=TRUE)} 
}

\examples{
logo <- stack(system.file("external/rlogo.grd", package="raster")) 
density(logo)
}

\keyword{spatial}
