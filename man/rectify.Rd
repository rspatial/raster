\name{rectify}

\alias{rectify}
\alias{rectify,Raster-method}


\title{rectify a Raster object}


\description{
rectify changes a rotated Raster* object into a non-rotated (rectangular) object. This is wrapper function around \code{\link{resample}}.
}


\usage{
\S4method{rectify}{Raster}(x, ext, res, method='ngb', filename='', ...) 
}

\arguments{
  \item{x}{Raster* object to be rectified}
  \item{ext}{Optional. Extent object or object from which an Extent object can be extracted}
  \item{res}{Optional. Single or two numbers to set the resolution}
  \item{method}{Method used to compute values for the new RasterLayer, should be "bilinear" for bilinear interpolation, or "ngb" for nearest neighbor }
  \item{filename}{Character. Output filename }
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}  
}


\value{
RasterLayer or RasterBrick object
}


\keyword{spatial}

