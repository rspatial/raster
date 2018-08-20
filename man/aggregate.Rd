\name{aggregate}

\docType{methods}

\alias{aggregate}
\alias{aggregate,Raster-method}
\alias{aggregate,SpatialPolygons-method}
\alias{aggregate,SpatialLines-method}


\title{Aggregate raster cells or SpatialPolygons/Lines}

\description{
Raster* objects: 

Aggregate a Raster* object to create a new RasterLayer or RasterBrick with a lower resolution (larger cells). 
Aggregation groups rectangular areas to create larger cells. The value for the resulting cells is computed with a user-specified function. 

SpatialPolygon*:

Aggregate a SpatialPolygon* object, optionally by combining polygons that have the same attributes for one or more variables. If the polygons touch or overlap, internal boundaries are optinally "dissolved".
}

\usage{
\S4method{aggregate}{Raster}(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename='', ...)

\S4method{aggregate}{SpatialPolygons}(x, by, sums, dissolve=TRUE, vars=NULL, ...)
}

\arguments{
  \item{x}{Raster* object or SpatialPolygons* object}
  \item{fact}{integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor) or three integers (when also aggregating over layers). See Details}  
  \item{fun}{function used to aggregate values }  
  \item{expand}{logical. If \code{TRUE} the output Raster* object will be larger then the input Raster* object if a division of the number of columns or rows with \code{factor} is not an integer}
  \item{na.rm}{logical. If \code{TRUE}, NA cells are removed from calculations }
  \item{filename}{character. Output filename (optional)}
  \item{...}{if \code{x} is a Raster* object, additional arguments as for \code{\link{writeRaster}}}
  
  \item{by}{character or integer. The variables (column names or numbers) that should be used to aggregate (dissolve) the SpatialPolygons by only maintaining unique combinations of these variables. The default setting is to use no variables and aggregate all polygons. You can also supply a vector with a length of length(x)}

  \item{sums}{list with function(s) and variable(s) to summarize. This should be a list of lists in which each element of the main lists has two items. The first item is function (e.g. mean), the second element is a vector of column names (or indices) that need to summarize with that function. Be careful with character and factor variables (you can use, e.g. 'first' \code{function(x)x[1]} or 'last' \code{function(x)x[length(x)]} or \code{modal} for these variables}
  \item{vars}{deprecated. Same as \code{by}}  
  \item{dissolve}{logical. If \code{TRUE} borders between touching or overlapping polygons are removed (requires package rgeos)}  
}


\details{
Aggregation of a \code{x} will result in a Raster* object with fewer cells. The number of cells is the number of cells of \code{x} divided by \code{fact*fact} (when fact is a single number) or \code{prod(fact)} (when fact consists of 2 or 3 numbers). If necessary this number is adjusted according to the value of \code{expand}. For example, \code{fact=2} will result in a new Raster* object with \code{2*2=4} times fewer cells. If two numbers are supplied, e.g., \code{fact=c(2,3)}, the first will be used for aggregating in the horizontal direction, and the second for aggregating in the vertical direction, and the returned object will have \code{2*3=6} times fewer cells. Likewise, \code{fact=c(2,3,4)} aggregates cells in groups of 2 (rows) by 3 (columns) and 4 (layers).
 
Aggregation starts at the upper-left end of a raster (you can use \code{\link{flip}} if you want to start elsewhere). If a division of the number of columns or rows with \code{factor} does not return an integer, the extent of the resulting Raster object will either be somewhat smaller or somewhat larger then the original RasterLayer. For example, if an input RasterLayer has 100 columns, and \code{fact=12}, the output Raster object will have either 8 columns (\code{expand=FALSE}) (using \code{8 x 12 = 96} of the original columns) or 9 columns (\code{expand=TRUE}). In both cases, the maximum x coordinate of the output RasterLayer would, of course, also be adjusted.
  
The function \code{fun} should take multiple numbers, and return a single number. For example \code{mean}, \code{modal}, \code{min} or \code{max}. 
It should also accept a \code{na.rm} argument (or ignore it as one of the 'dots' arguments). 
}

\value{
RasterLayer or RasterBrick, or a SpatialPolygons* object
}

\seealso{ \code{\link{disaggregate}}, \code{\link{resample}}. For SpatialPolygons* \code{\link[sp]{disaggregate}} }

\author{Robert J. Hijmans and Jacob van Etten}

\examples{
r <- raster()
# a new aggregated raster, no values
ra <- aggregate(r, fact=10)
r <- setValues(r, runif(ncell(r)))

# a new aggregated raster, max of the values
ra <- aggregate(r, fact=10, fun=max)

# multiple layers
s <- stack(r, r*2)
x <- aggregate(s,2)

#SpatialPolygons
if (require(rgdal) & require(rgeos)) {
	p <- shapefile(system.file("external/lux.shp", package="raster"))
	p
	pa0 <- aggregate(p)
	pa0
	pa1 <- aggregate(p, by='NAME_1', sums=list(list(mean, 'ID_2')))
	pa1
}
}

\keyword{methods}
\keyword{spatial}

