\name{pointDistance}

\alias{pointDistance}

\title{Distance between points}

\description{
Calculate the geographic distance between two (sets of) points on the WGS ellipsoid (\code{lonlat=TRUE}) or on a plane (\code{lonlat=FALSE}). If both sets do not have the same number of points, the distance between each pair of points is given. If both sets have the same number of points, the distance between each point and the corresponding point in the other set is given, except if \code{allpairs=TRUE}.
}

\usage{
pointDistance(p1, p2, lonlat, allpairs=FALSE, ...)
}

\arguments{
\item{p1}{x and y coordinate of first (set of) point(s), either as c(x, y), matrix(ncol=2), or SpatialPoints*. }
\item{p2}{x and y coordinate of second (set of) second point(s) (like for \code{p1}). If this argument is missing, a distance matrix is computed for \code{p1} }
\item{lonlat}{logical. If \code{TRUE}, coordinates should be in degrees; else they should represent planar ('Euclidean') space (e.g. units of meters) }
\item{allpairs}{logical. Only relevant if the number of points in \code{x} and \code{y} is the same. If \code{FALSE} the distance between each point in \code{x} with the corresponding point in \code{y} is returned. If \code{TRUE} a full distance matrix is returned }
\item{...}{Additional arguments. None implemented }
}

\value{
A single value, or a vector, or matrix of values giving the distance in meters (lonlat=TRUE) or map-units (for instance, meters in the case of UTM)
If \code{p2} is missing, a distance matrix is returned
}

\seealso{\code{\link{distanceFromPoints}, \link{distance}, \link{gridDistance}}, \code{\link[sp]{spDistsN1}}. 
The \code{geosphere} package has many additional distance functions and other functions that operate on spherical coordinates}


\author{Robert J. Hijmans and Jacob van Etten. The distance for longitude/latitude data uses GeographicLib by C.F.F. Karney}

\examples{
a <- cbind(c(1,5,55,31),c(3,7,20,22))
b <- cbind(c(4,2,8,65),c(50,-90,20,32))   

pointDistance(c(0, 0), c(1, 1), lonlat=FALSE)
pointDistance(c(0, 0), c(1, 1), lonlat=TRUE)
pointDistance(c(0, 0), a, lonlat=TRUE)
pointDistance(a, b, lonlat=TRUE)
   
#Make a distance matrix 
dst <- pointDistance(a, lonlat=TRUE)
# coerce to dist object
dst <- as.dist(dst)
}

\keyword{ spatial }
