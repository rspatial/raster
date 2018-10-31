\name{as.character}

\alias{as.character}
\alias{as.character,Raster-method}
\alias{as.character,Extent-method}
  
 
\title{Character representation of a Raster or Extent object}

\description{
\code{as.character} returns a text (R code) representation of a Raster* or Extent object. The main purpose of this is to allow quick generation of objects to use in examples on, for example, stackoverflow.com. 
}

\usage{
\S4method{as.character}{Raster}(x, ...)
\S4method{as.character}{Extent}(x, ...)
}

\arguments{
 \item{x}{ Raster* or Extent object }
 \item{...}{ additional arguments, none implemented } 
}
  

\value{
character
}

\examples{
r <- raster(ncol=3, nrow=3)
values(r) <- 1:ncell(r)
as.character(r)
s <- stack(r, r)
as.character(s)
as.character(extent(s))

x <- as.character(s)
eval(parse(text=x))

y <- as.character(extent(s))
eval(parse(text=y))
}

\keyword{spatial}
\keyword{methods}
