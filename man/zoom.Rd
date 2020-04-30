\name{zoom}

\docType{methods}

\alias{zoom}
\alias{zoom,Raster-method}
\alias{zoom,Spatial-method}
\alias{zoom,missing-method}


\title{Zoom in on a map}


\description{
Zoom in on a map (plot) by providing a new extent, by default this is done by clicking twice on the map.
}

\usage{
zoom(x, ...) 
\S4method{zoom}{Raster}(x, ext=drawExtent(), maxpixels=100000, layer=1, new=TRUE, useRaster=TRUE, ...)

\S4method{zoom}{Spatial}(x, ext=drawExtent(), new=TRUE, ...)

\S4method{zoom}{missing}(x, ext=drawExtent(), new=TRUE, ...)
}

\arguments{
\item{x}{Raster* or Spatial* (vector type) object}
\item{ext}{Extent object, or other object from which an extent can be extracted}
\item{maxpixels}{Maximum number of pixels used for the map}
\item{layer}{Positive integer to select the layer to be used if x is a mutilayer Raster object}
\item{new}{Logical. If \code{TRUE}, the zoomed in map will appear on a new device (window)}
\item{useRaster}{Logical. If \code{TRUE}, a bitmap raster is used to plot the image instead of polygons}
\item{...}{additional paramters for \code{\link[base]{plot}}}
}

\value{
Extent object (invisibly)
}



\seealso{ \code{\link[raster]{drawExtent}}, \code{\link[raster]{plot}}}

\keyword{spatial}

