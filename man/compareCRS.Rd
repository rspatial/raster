\name{compareCRS}

\alias{compareCRS}


\title{ Partially compare two CRS objects }

\description{
Compare CRS objects 
}

\usage{
compareCRS(x, y, unknown=FALSE, verbatim=FALSE, verbose=FALSE)
}

\arguments{
  \item{x}{CRS object, or object from which it can be extracted with \code{\link{projection}}, or PROJ.4 format character string}
  \item{y}{same as \code{x}}
  \item{unknown}{logical. Return \code{TRUE} if \code{x} or \code{y} is \code{TRUE}}
  \item{verbatim}{logical. If \code{TRUE} compare \code{x} and \code{y}, verbatim (not partially)}
  \item{verbose}{logical. If \code{TRUE}, messages about the comparison may be printed}
}


\value{
logical
}


\seealso{\code{sp::identicalCRS},  \code{\link{crs} }}


\examples{ 
compareCRS("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84",
  "+proj=longlat +datum=WGS84")
compareCRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
  "+proj=longlat +datum=WGS84")
compareCRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
  "+proj=longlat +datum=WGS84", verbatim=TRUE)
compareCRS("+proj=longlat +datum=WGS84", NA)
compareCRS("+proj=longlat +datum=WGS84", NA, unknown=TRUE)
}

\keyword{ spatial }

