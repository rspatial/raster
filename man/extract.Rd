\name{extract}

\docType{methods}

\alias{extract}
\alias{extract,Raster,vector-method}
\alias{extract,Raster,matrix-method}
\alias{extract,Raster,data.frame-method}
\alias{extract,Raster,SpatialPoints-method}
\alias{extract,Raster,SpatialLines-method}
\alias{extract,Raster,SpatialPolygons-method}
\alias{extract,Raster,sf-method}
\alias{extract,Raster,Extent-method}
\alias{extract,SpatialPolygons,SpatialPoints-method}
\alias{extract,SpatialPolygons,data.frame-method}
\alias{extract,SpatialPolygons,matrix-method}

\title{Extract values from Raster objects}

\description{
Extract values from a Raster* object at the locations of spatial vector data. There are methods for points, lines, and polygons (classes from `sp` or `sf`), for a matrix or data.frame of points. You can also use cell numbers and Extent (rectangle) objects to extract values. 

If \code{y} represents points, \code{extract} returns the values of a Raster* object for the cells in which a set of points fall.
If \code{y} represents lines, the \code{extract} method returns the values of the cells of a Raster* object that are touched by a line. If \code{y} represents polygons, the \code{extract} method returns the values of the cells of a Raster* object that are covered by a polygon. A cell is covered if its center is inside the polygon (but see the \code{weights} option for considering partly covered cells; and argument \code{small} for getting values for small polygons).

It is also possible to extract values for point locations from SpatialPolygons.
}

\usage{
\S4method{extract}{Raster,matrix}(x, y, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
   fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE, ...)

\S4method{extract}{Raster,SpatialLines}(x, y, fun=NULL, na.rm=FALSE, cellnumbers=FALSE, df=FALSE, layer,
   nl, factors=FALSE, along=FALSE, sp=FALSE, ...)

\S4method{extract}{Raster,SpatialPolygons}(x, y, fun=NULL, na.rm=FALSE, exact=FALSE,  
   normalizeWeights=TRUE, cellnumbers=FALSE, small=TRUE, df=FALSE, layer, nl, 
   factors=FALSE, sp=FALSE, weights=FALSE, ...)


\S4method{extract}{SpatialPolygons,SpatialPoints}(x, y, ...)
}

\arguments{
\item{x}{Raster* object}

\item{y}{points represented by a two-column matrix or data.frame, or \code{\link{SpatialPoints}*}; \code{\link{SpatialPolygons}*}; \code{\link[sp]{SpatialLines}}; \code{sf} spatial vector objects; \code{\link{Extent}}; or a numeric vector representing cell numbers}

\item{method}{character. \code{'simple'} or \code{'bilinear'}. If \code{'simple'} values for the cell a point falls in are returned. If \code{'bilinear'} the returned values are interpolated from the values of the four nearest raster cells.}

\item{buffer}{numeric. The radius of a buffer around each point from which to extract cell values. If the distance between the sampling point and the center of a cell is less than or equal to the buffer, the cell is included. The buffer can be specified as a single value, or as a vector of the length of the number of points.  If the data are not projected (latitude/longitude), the unit should be meters. Otherwise it should be in map-units (typically also meters).}  

\item{small}{logical. If \code{TRUE} and \code{y} represents points and a \code{buffer} argument is used, the function always return a number, also when the buffer does not include the center of a single cell. The value of the cell in which the point falls is returned if no cell center is within the buffer. If \code{y} represents polygons, a value is also returned for relatively small polygons (e.g. those smaller than a single cell of the Raster* object), or polygons with an odd shape, for which otherwise no values are returned because they do not cover any raster cell centers. In some cases, you could alternatively use the centroids of such polygons, for example using \code{extract(x, coordinates(y))} or \code{extract(x, coordinates(y), method='bilinear')}.}

\item{fun}{function to summarize the values (e.g. \code{mean}). The function should take a single numeric vector as argument and return a single value (e.g. mean, min or max), and accept a \code{na.rm} argument. Thus, standard R functions not including an na.rm argument must be wrapped as in this example: fun=function(x,...)length(x). If \code{y} represents points, \code{fun} is only used when a buffer is used (and hence multiple values per spatial feature would otherwise be returned).} 

\item{na.rm}{logical. Only useful when an argument \code{fun} is supplied. If \code{na.rm=TRUE} (the default value), NA values are removed before fun is applied. This argument may be ignored if the function used has a \code{...} argument and ignores an additional \code{na.rm} argument}

\item{cellnumbers}{logical. If \code{cellnumbers=TRUE}, cell-numbers will also be returned (if no \code{fun} argument is  supplied, and when extracting values with points, if \code{buffer} is \code{NULL})}

\item{df}{logical. If \code{df=TRUE}, results will be returned as a data.frame. The first column is a sequential ID, the other column(s) are the extracted values}

\item{exact}{logical. If \code{TRUE} the function returns, for each polygon, a matrix with the cell values and the fraction of each cell that is covered by the polygon. The weights can be used for averaging; see examples. This option is particularly useful if the polygons are small relative to the cells size of the Raster* object}

\item{normalizeWeights}{logical. If \code{TRUE}, weights are normalized such that they add up to one for each polygon}

\item{factors}{logical. If \code{TRUE}, factor values are returned, else their integer representation is returned}

\item{layer}{integer. First layer for which you want values (if \code{x} is a multilayer object)}

\item{nl}{ integer. Number of layers for which you want values (if \code{x} is a multilayer object)}

\item{along}{ boolean. Should returned values be ordered to go along the lines?}

\item{sp}{ boolean. Should the extracted values be added to the data.frame of the Spatial* object \code{y}? This only applies if \code{y} is a Spatial* object and, for SpatialLines and SpatialPolygons, if \code{fun} is not NULL. In this case the returned value is the expanded Spatial object}

\item{weights}{logical. If \code{TRUE} and \code{normalizeWeights=FALSE}, the function returns, for each polygon, a matrix with the cell values and the approximate fraction of each cell that is covered by the polygon(rounded to 1/100) --- to be depracated, use "exact" instead}

\item{...}{additional arguments (none implemented)}  
}

\value{
A vector for RasterLayer objects, and a matrix for RasterStack or RasterBrick objects. A list (or a data.frame if \code{df=TRUE}) if \code{y} is a SpatialPolygons* or SpatialLines* object or if a \code{buffer} argument is used (but not a \code{fun} argument). If \code{sp=TRUE} and  \code{y} is a Spatial* object and  \code{fun} is not NULL a Spatial* object is returned. The order of the returned values corresponds to the order of object \code{y}. If \code{df=TRUE}, this is also indicated in the first variable ('ID').
}

\seealso{ \code{\link{getValues}, \link{getValuesFocal}}  }

\examples{
r <- raster(ncol=36, nrow=18, vals=1:(18*36))

###############################
# extract values by cell number
###############################
extract(r, c(1:2, 10, 100))
s <- stack(r, sqrt(r), r/r)
extract(s, c(1, 10, 100), layer=2, n=2)

###############################
# extract values with points
###############################
xy <- cbind(-50, seq(-80, 80, by=20))
extract(r, xy)

sp <- SpatialPoints(xy)
extract(r, sp, method='bilinear')

# examples with a buffer
extract(r, xy[1:3,], buffer=1000000)
extract(r, xy[1:3,], buffer=1000000, fun=mean)

## illustrating the varying size of a buffer (expressed in meters) 
## on a longitude/latitude raster
 z <- extract(r, xy, buffer=1000000)
 s <- raster(r)
 for (i in 1:length(z)) { s[z[[i]]] <- i }
 
## compare with raster that is not longitude/latitude
 crs(r) <- "+proj=utm +zone=17" 
 xy[,1] <- 50
 z <- extract(r, xy, buffer=8)
 for (i in 1:length(z)) { s[z[[i]]] <- i }
 plot(s)
# library(maptools)
# data(wrld_simpl)
# plot(wrld_simpl, add=TRUE)

###############################
# extract values with lines
###############################
r <- raster(ncol=36, nrow=18, vals=1:(18*36))
cds1 <- rbind(c(-50,0), c(0,60), c(40,5), c(15,-45), c(-10,-25))
cds2 <- rbind(c(80,20), c(140,60), c(160,0), c(140,-55))
lines <- spLines(cds1, cds2)

extract(r, lines)

###############################
# extract values with polygons
###############################
cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys <- spPolygons(cds1, cds2)

v <- extract(r, polys)
# mean for each polygon
unlist(lapply(v, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))

# v <- extract(r, polys, cellnumbers=TRUE)

# weighted mean
# v <- extract(r, polys, weights=TRUE, fun=mean)
# equivalent to:
# v <- extract(r, polys, weights=TRUE)
# sapply(v, function(x) if (!is.null(x)) {sum(apply(x, 1, prod)) / sum(x[,2])} else NA)


###############################
# extract values with an extent
###############################
e <- extent(150,170,-60,-40)
extract(r, e)
#plot(r)
#plot(e, add=T)
}

\keyword{methods}
\keyword{spatial}

