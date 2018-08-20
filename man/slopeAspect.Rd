\name{Slope and aspect}

\alias{slopeAspect}

\title{Slope and aspect}

\description{
This is a deprecated function. Use \code{\link{terrain}} instead. 
}

\usage{
slopeAspect(dem, filename='', out=c('slope', 'aspect'), unit='radians', 
                 neighbors=8, flatAspect, ...)  
}

\arguments{
  \item{dem}{RasterLayer object with elevation values in map units, or in meters when the crs is longitude/latitude}
  \item{filename}{Character. Filename. optional}
  \item{out}{Character vector containing one or more of these options: 'slope', 'aspect'}
  \item{unit}{Character. 'degrees' or 'radians' }
  \item{neighbors}{Integer. Indicating how many neighboring cells to use to compute slope for any cell. Either 8 (queen case) or 4 (rook case), see Details}
  \item{flatAspect}{Numeric or NA. What value to use for aspect when slope is zero (and hence the aspect is undefined)? The default value is 90 degrees (or 0.5*pi radians) }
  \item{...}{Standard additional arguments for writing RasterLayer files}
}

\seealso{ \code{\link{terrain}} }



\keyword{spatial}
