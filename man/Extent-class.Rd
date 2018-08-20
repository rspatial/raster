\name{Extent-class}

\docType{class}

\alias{Extent}
\alias{Extent-class}
\alias{show,Extent-method}

\title{Class "Extent" }

\description{
Objects of class Extent are used to define the spatial extent (extremes) of objects of the BasicRaster and Raster* classes. 
}

\section{Objects from the Class}{
You can use the \code{\link{extent}} function to create Extent objects, or to extract them from Raster* and Spatial* objects.
}

\section{Slots}{
	 \describe{
    \item{\code{xmin}:}{minimum x coordinate}
    \item{\code{xmax}:}{maximum x coordinate}
    \item{\code{ymin}:}{minumum y coordinate}
    \item{\code{ymax}:}{maximum y coordinate}
  }
}

\section{Methods}{
  \describe{
 \item{show}{display values of a Extent object }
 }
}




\seealso{ \code{\link{extent}}, \code{\link[raster]{setExtent}} }

\examples{
ext <- extent(-180,180,-90,90)
ext
}

\keyword{classes}
\keyword{spatial}
