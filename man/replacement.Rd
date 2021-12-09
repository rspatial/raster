\name{replacement}

\docType{methods}


\alias{[<-,RasterLayer,RasterLayer,missing-method}
\alias{[<-,RasterLayer,missing,missing-method}
\alias{[<-,RasterStackBrick,Raster,missing-method}
\alias{[<-,RasterStackBrick,missing,missing-method}

\alias{[[<-,RasterStack,numeric,missing-method}
\alias{[[<-,Raster,numeric,missing-method}
\alias{[[<-,RasterStackBrick,character,missing-method}
\alias{[[<-,RasterLayer,character,missing-method}

\alias{[<-,Raster,numeric,numeric-method}
\alias{[<-,Raster,numeric,missing-method}
\alias{[<-,Raster,matrix,missing-method}
\alias{[<-,Raster,logical,missing-method}
\alias{[<-,Raster,missing,numeric-method}
\alias{[<-,Raster,Spatial,missing-method}
\alias{[<-,Raster,Extent,missing-method}
\alias{[<-,Extent,numeric,missing-method}

\alias{$<-,Raster-method}
\alias{$,Raster-method}


\title{Replace cell values or layers of a Raster* object}

\description{
You can set values of a Raster* object, when \code{i} is a vector of cell numbers, a Raster*, Extent, or Spatial* object. 

These are shorthand methods that work best for relatively small Raster* objects. In other cases you can use functions such as \code{\link{calc} and \link{rasterize}}. 
}

\section{Methods}{
\describe{
\code{x[i] <- value}

\code{x[i,j] <- value}

\tabular{rll}{
\tab \bold{Arguments:} \tab  \cr
\tab \code{x} \tab a Raster* object \cr
\tab \code{i} \tab cell number(s), row number(s), Extent, Spatial* object \cr
\tab \code{j} \tab columns number(s) (only available if i is (are) a row number(s)) \cr
\tab \code{value} \tab new cell value(s) \cr
}
}}

\seealso{ \link{calc}, \link{rasterize}} 


\examples{
r <- raster(ncol=10, nrow=5)
values(r) <- 1:ncell(r) * 2
r[1,] <- 1
r[,1] <- 2
r[1,1] <- 3

s <- stack(r, sqrt(r))
s[s<5] <- NA

}

\keyword{methods}
\keyword{spatial}
