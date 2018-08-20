\name{spplot}

\docType{methods}

\alias{spplot}
\alias{spplot,Raster-method}
\alias{spplot,SpatialPoints-method}
\alias{lines,SpatialPolygons-method}


\title{Use spplot to plot a Raster* object}

\description{
A wrapper function around \link[sp]{spplot} (sp package). With spplot it is easy to map several layers with a single legend for all maps. ssplot is itself a wrapper around the \link[lattice]{levelplot} function in the lattice package, and see the help for these functions for additional options.

One of the advantages of these wrapper functions is the additional \code{maxpixels} argument to sample large Raster objects for faster drawing.
}

\section{Methods}{
\describe{
\code{spplot(obj, ..., maxpixels=50000, as.table=TRUE)}

\tabular{rll}{
\tab \code{obj} \tab  A Raster* object  \cr
\tab \code{...} \tab  Any argument that can be passed to \code{\link[sp]{spplot}} and \link[lattice]{levelplot} \cr
\tab \code{maxpixels} \tab Integer. Number of pixels to sample from each layer of large Raster objects \cr
}
}}


\seealso{
\code{ \link[raster]{plot}, \link[raster]{plotRGB} }

The rasterVis package has more advanced plotting methods for Raster objects
}



\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
s <- stack(r, r*2)
names(s) <- c('meuse', 'meuse x 2')

spplot(s)

pts <- data.frame(sampleRandom(r, 10, xy=TRUE))
coordinates(pts) <- ~ x + y

spplot(s, scales = list(draw = TRUE), 
		xlab = "easting", ylab = "northing", 
		col.regions = rainbow(99, start=.1), 
		names.attr=c('original', 'times two'),
		sp.layout = list("sp.points", pts, pch=20, cex=2, col='black'),
		par.settings = list(fontsize = list(text = 12)), at = seq(0, 4000, 500))

}

\keyword{methods}
\keyword{spatial}

