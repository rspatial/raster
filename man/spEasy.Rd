\name{spEasy}

\alias{spLines}
\alias{spPolygons}
  
\title{Create SpatialLines* or SpatialPolygons*}

\description{
Helper functions to simplify the creation of SpatialLines* or SpatialPolygons* objects from coordinates.
}

\usage{
spLines(x, ..., attr=NULL, crs="") 
spPolygons(x, ..., attr=NULL, crs="") 
}

\arguments{
\item{x}{matrix of list with matrices. Each matrix must have two columns with x and y coordinates (or longitude and latitude, in that order). Multi-line or multi-polygon objects can be formed by combining matrices in a list}
\item{...}{additional matrices and/or lists with matrices}  
\item{attr}{data.frame with the attributes to create a *DataFrame object. The number of rows must match the number of lines/polgyons}
\item{crs}{the coordinate reference system (PROJ4 or WKT notation)}
}

\value{
SpatialLines* or SpatialPolygons*
}

\examples{
x1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60))
x2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55))
x3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45))
x4 <- rbind(c(41,-41.5), c(51,-35), c(62,-41), c(51,-50))

a <- spLines(x1, x2, x3)
b <- spLines(x1, list(x2, x3), attr=data.frame(id=1:2), crs='+proj=longlat +datum=WGS84')
b

hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-130,10))
d <- spPolygons(list(x1,hole), x2, list(x3, x4))

att <- data.frame(ID=1:3, name=c('a', 'b', 'c'))
e <- spPolygons(list(x1,hole), x2, list(x3, x4), attr=att, crs='+proj=longlat +datum=WGS84')
e
}

\keyword{spatial}
