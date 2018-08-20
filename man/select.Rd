\name{select}

\docType{methods}

\alias{select}
\alias{select,Raster-method}
\alias{select,Spatial-method}

\title{
Geometric subsetting
}

\description{
Geometrically subset Raster* or Spatial* objects by drawing on a plot (map).
}


\usage{
\S4method{select}{Raster}(x, use='rec', ...)

\S4method{select}{Spatial}(x, use='rec', draw=TRUE, col='cyan', size=2, ...)
}

\arguments{
\item{x}{Raster*, SpatialPoints*, SpatialLines*, or SpatialPolygons*}
\item{use}{character: 'rec' or 'pol'. To use a rectangle or a polygon for selecting}
\item{draw}{logical. Add the selected features to the plot?}
\item{col}{color to use to draw the selected features (when \code{draw=TRUE)}}
\item{size}{integer > 0. Size to draw the selected features with (when \code{draw=TRUE)})}
\item{...}{additional arguments. None implemented}
}

\seealso{
\code{\link{click}, \link{crop}}
}


\value{
Raster* or Spatial* object
}


\examples{
\dontrun{

# select a subset of a RasterLayer
r <- raster(nrow=10, ncol=10)
r[] <- 1:ncell(r)
plot(r)
s <- select(r) # now click on the map twice

# plot the selection on a new canvas:
x11()
plot(s)


# select a subset of a SpatialPolygons object
p1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))
p2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55), c(-10,0))
p3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45), c(-125,0))
pols <- SpatialPolygons( list(  Polygons(list(Polygon(p1), Polygon(hole)), 1),
      Polygons(list(Polygon(p2)), 2), Polygons(list(Polygon(p3)), 3)))
pols@polygons[[1]]@Polygons[[2]]@hole <- TRUE

plot(pols, col=rainbow(3))
ps <- select(pols) # now click on the map twice
ps
}
}

\keyword{spatial}

