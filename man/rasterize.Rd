\name{rasterize}

\docType{methods}

\alias{rasterize}
\alias{rasterize,matrix,Raster-method}
\alias{rasterize,data.frame,Raster-method}
\alias{rasterize,sf,Raster-method}
\alias{rasterize,SpatialPoints,Raster-method}
\alias{rasterize,SpatialLines,Raster-method}
\alias{rasterize,SpatialPolygons,Raster-method}
\alias{rasterize,Extent,Raster-method}

\title{Rasterize points, lines, or polygons}

\description{
Transfer values associated with 'object' type spatial data (points, lines, polygons) to raster cells.

For polygons, values are transferred if the polygon covers the center of a raster cell. For lines, values are transferred to all cells that are touched by a line. You can combine this behaviour by rasterizing polygons as lines first and then as polygons.

If \code{x} represents points, each point is assigned to a grid cell. Points that fall on a border between cells are placed in the cell to the right and/or in the cell below. The value of a grid cell is determined by the values associated with the points and function \code{fun}.
}

\usage{
\S4method{rasterize}{matrix,Raster}(x, y, field, fun='last', background=NA,
     mask=FALSE, update=FALSE, updateValue='all', filename="", na.rm=TRUE, ...)

\S4method{rasterize}{SpatialPoints,Raster}(x, y, field, fun='last', background=NA,
    mask=FALSE, update=FALSE, updateValue='all', filename="", na.rm=TRUE, ...)

\S4method{rasterize}{SpatialLines,Raster}(x, y, field, fun='last', background=NA,
    mask=FALSE, update=FALSE, updateValue='all', filename="", ...)

\S4method{rasterize}{SpatialPolygons,Raster}(x, y, field, fun='last', background=NA,
    mask=FALSE, update=FALSE, updateValue='all', filename="",
    getCover=FALSE, silent=TRUE, ...)
}

\arguments{
\item{x}{points (a SpatialPoints* object, or a two-column matrix (or data.frame)), SpatialLines*, SpatialPolygons*, or an Extent object }

\item{y}{Raster* object}

\item{field}{numeric or character. The value(s) to be transferred. This can be a single number, or a vector of numbers that has the same length as the number of spatial features (points, lines, polygons). If \code{x} is a Spatial*DataFrame, this can be the column name of the variable to be transferred. If missing, the attribute index is used (i.e. numbers from 1 to the number of features). You can also provide a vector with the same length as the number of spatial features, or a matrix where the number of rows matches the number of spatial features}

\item{fun}{function or character. To determine what values to assign to cells that are covered by multiple spatial features. You can use functions such as \code{min, max}, or \code{mean}, or one of the following character values: \code{'first'}, \code{'last'}, \code{'count'}. The default value is \code{'last'}. In the case of SpatialLines*, \code{'length'} is also allowed (currently for planar coordinate systems only).


If \code{x} represents points, \code{fun} must accept a \code{na.rm} argument, either explicitly or through 'dots'. This means that \code{fun=length} fails, but \code{fun=function(x,...)length(x)} works, although it ignores the \code{na.rm} argument. To use the \code{na.rm} argument you can use a function like this: fun=function(x, na.rm){if (na.rm) length(na.omit(x)) else (length(x)}, or use a function that removes \code{NA} values in all cases, like this function to compute the number of unique values per grid cell "richness": \code{fun=function(x, ...) {length(unique(na.omit(x)))} }. If you want to count the number of points in each grid cell, you can use \code{ fun='count'} or  \code{fun=function(x,...){length(x)}}.

You can also pass multiple functions using a statement like \code{fun=function(x, ...) c(length(x),mean(x))}, in which case the returned object is a RasterBrick (multiple layers).
}

\item{background}{numeric. Value to put in the cells that are not covered by any of the features of \code{x}. Default is \code{NA}}

\item{mask}{logical. If \code{TRUE} the values of the input Raster object are 'masked' by the spatial features of \code{x}. That is, cells that spatially overlap with the spatial features retain their values, the other cells become \code{NA}. Default is \code{FALSE}. This option cannot be used when \code{update=TRUE}}

\item{update}{logical. If \code{TRUE}, the values of the Raster* object are updated for the cells that overlap the spatial features of \code{x}. Default is \code{FALSE}. Cannot be used when \code{mask=TRUE}}

\item{updateValue}{numeric (normally an integer), or character. Only relevant when \code{update=TRUE}. Select, by their values, the cells to be updated with the values of the spatial features. Valid character values are \code{'all'}, \code{'NA'}, and \code{'!NA'}. Default is \code{'all'}}

\item{filename}{character. Output filename (optional)}

\item{na.rm}{If \code{TRUE}, \code{NA} values are removed if \code{fun} honors the \code{na.rm} argument}

\item{getCover}{logical. If \code{TRUE}, the fraction of each grid cell that is covered by the polygons is returned (and the values of \code{field, fun, mask}, and \code{update} are ignored. The fraction covered is estimated by dividing each cell into 100 subcells and determining presence/absence of the polygon in the center of each subcell}

\item{silent}{Logical. If \code{TRUE}, feedback on the polygon count is suppressed. Default is \code{FALSE}}

\item{...}{Additional arguments for file writing as for \code{\link{writeRaster}}}
}


\value{
RasterLayer or RasterBrick
}

\seealso{ \code{\link{extract}} }

\examples{

###############################
# rasterize points
###############################
r <- raster(ncols=36, nrows=18)
n <- 1000
set.seed(123)
x <- runif(n) * 360 - 180
y <- runif(n) * 180 - 90
xy <- cbind(x, y)
# get the (last) indices
r0 <- rasterize(xy, r)
# presence/absensce (NA) (is there a point or not?)
r1 <- rasterize(xy, r, field=1)
# how many points?
r2 <- rasterize(xy, r, fun=function(x,...)length(x))
vals <- runif(n)
# sum of the values associated with the points
r3 <- rasterize(xy, r, vals, fun=sum)

# with a SpatialPointsDataFrame
vals <- 1:n
p <- data.frame(xy, name=vals)
coordinates(p) <- ~x+y
r <- rasterize(p, r, 'name', fun=min)
#r2 <- rasterize(p, r, 'name', fun=max)
#plot(r, r2, cex=0.5)

###############################
# rasterize lines
###############################
cds1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60))
cds2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55))
cds3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45))

lines <- spLines(cds1, cds2, cds3)

r <- raster(ncols=90, nrows=45)
r <- rasterize(lines, r)

\dontrun{
plot(r)
plot(lines, add=TRUE)

r <- rasterize(lines, r, fun='count')
plot(r)

r[] <- 1:ncell(r)
r <- rasterize(lines, r, mask=TRUE)
plot(r)

r[] <- 1
r[lines] <- 10
plot(r)
}

###############################
# rasterize polygons
###############################

p1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))
p1 <- list(p1, hole)
p2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55), c(-10,0))
p3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45), c(-125,0))

pols <- spPolygons(p1, p2, p3)

r <- raster(ncol=90, nrow=45)
r <- rasterize(pols, r, fun=sum)


\dontrun{

plot(r)
plot(pols, add=T)

# add a polygon
p5 <- rbind(c(-180,10), c(0,90), c(40,90), c(145,-10),
            c(-25, -15), c(-180,0), c(-180,10))
addpoly <- SpatialPolygons(list(Polygons(list(Polygon(p5)), 1)))
addpoly <- as(addpoly, "SpatialPolygonsDataFrame")
addpoly@data[1,1] <- 10
r2 <- rasterize(addpoly, r, field=1, update=TRUE, updateValue="NA")
plot(r2)
plot(pols, border="blue", lwd=2, add=TRUE)
plot(addpoly, add=TRUE, border="red", lwd=2)

# get the percentage cover of polygons in a cell
r3 <- raster(ncol=36, nrow=18)
r3 <- rasterize(pols, r3, getCover=TRUE)
}
}

\keyword{methods}
\keyword{spatial}
