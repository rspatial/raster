\name{as.raster}

\alias{as.raster}
\alias{as.raster,RasterLayer-method}
  
\title{Coerce to a 'raster' object}

\description{
Implementation of the generic \code{\link[grDevices]{as.raster}} function to create a 'raster' (small r) object. NOT TO BE CONFUSED with the Raster* (big R) objects defined by the raster package! Such objects can be used for plotting with the \code{\link[graphics]{rasterImage}} function. 
}

\usage{
as.raster(x, ...)
}

\arguments{
 \item{x}{ RasterLayer object }
 \item{...}{ Additional arguments. 
 
   \code{maxpixels} Integer. To regularly subsample very large objects 
	
   \code{col} Vector of colors. Default is col=rev(terrain.colors(255)))
	}
}
 

\value{
'raster' object
}



\examples{
r <- raster(ncol=3, nrow=3)
r[] <- 1:ncell(r)
as.raster(r)
}

\keyword{spatial}
\keyword{methods}
