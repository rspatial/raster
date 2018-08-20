\name{weighted.mean}

\alias{weighted.mean}
\alias{weighted.mean,RasterStackBrick,vector-method}
\alias{weighted.mean,RasterStackBrick,RasterStackBrick-method}


\title{Weighted mean of rasters}


\description{
Computes the weighted mean for each cell of a number or raster layers. The weights can be spatially variable or not.
}


\usage{
\S4method{weighted.mean}{RasterStackBrick,vector}(x, w, na.rm=FALSE, filename='', ...)

\S4method{weighted.mean}{RasterStackBrick,RasterStackBrick}(x, w, na.rm=FALSE,filename='', ...)
}


\arguments{
  \item{x}{RasterStack or RasterBrick}

  \item{w}{A vector of weights (one number for each layer), or for spatially variable weights, a RasterStack or RasterBrick with weights (should have the same extent, resolution and number of layers as x)}

  \item{na.rm}{Logical. Should missing values be removed?}

  \item{filename}{Character. Output filename (optional)}
  
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}
}

\value{
RasterLayer
}


\seealso{
\code{\link{Summary-methods}}, \code{\link[stats]{weighted.mean}}
}


\examples{
b <- brick(system.file("external/rlogo.grd", package="raster"))

# give least weight to first layer, most to last layer
wm1 <- weighted.mean(b, w=1:3)

# spatially varying weights
# weigh by column number
w1 <- init(b, v='col')

# weigh by row number
w2 <- init(b, v='row')
w <- stack(w1, w2, w2)

wm2 <- weighted.mean(b, w=w)
}

