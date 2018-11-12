\name{overlay}

\docType{methods}

\alias{overlay}
\alias{overlay,Raster,Raster-method}
\alias{overlay,Raster,missing-method}

\title{Overlay Raster objects}

\description{
Create a new Raster* object, based on two or more Raster* objects. (You can also use a single object, but perhaps \code{\link{calc}} is what you are looking for in that case).

You should supply a function \code{fun} to set the way that the RasterLayers are combined. The number of arguments in the function must match the number of Raster objects (or take any number). For example, if you combine two RasterLayers you could use multiply: \code{fun=function(x,y){return(x*y)}} percentage: \code{fun=function(x,y){return(100 * x / y)}}. If you combine three layers you could use \code{fun=function(x,y,z){return((x + y) * z)}}

Note that the function must work for vectors (not only for single numbers). That is, it must return the same number of elements as its input vectors. Alternatively, you can also supply a function such as \code{sum}, that takes \code{n} arguments (as \code{'...'}), and perhaps also has a \code{na.rm} argument, like in \code{sum(..., na.rm)}.

If a single mutli-layer object is provided, its layers are treated as individual RasterLayer objects if the argument \code{unstack=TRUE} is used. If multiple objects are provided, they should have the same number of layers, or it should be possible to recycle them (e.g., 1, 3, and 9 layers, which would return a RasterBrick with 9 layers).
}

\usage{
\S4method{overlay}{Raster,Raster}(x, y, ..., fun, filename="", recycle=TRUE, forcefun=FALSE)

\S4method{overlay}{Raster,missing}(x, y, ..., fun, filename="", unstack=TRUE, forcefun=FALSE)
}

\arguments{
  \item{x}{Raster* object}
  \item{y}{Raster* object, or missing (only useful if \code{x} has multiple layers)}
  \item{...}{Additional Raster objects (and/or arguments for writing files as in \code{\link{writeRaster})}}    
  \item{fun}{Function to be applied. When using RasterLayer objects, the number of arguments of the function should match the number of Raster objects, or it should take any number of arguments. When using multi-layer objects the function should match the number of layers of the RasterStack/Brick object (unless unstack=FALSE) }
  \item{filename}{Character. Output filename (optional) }
  \item{recycle}{Logical. Should layers from Raster objects with fewer layers be recycled?}  
  \item{unstack}{Logical. Should layers be unstacked before computation (i.e. does the \code{fun} refer to individual layers in a multilayer object)?}  
  \item{forcefun}{Boolean. If \code{TRUE}, overlay will not attempt to internally use apply (it is rarely necessary to use this argument)}
}


\details{
Instead of the overlay function you can also use arithmetic functions such as \code{*, /, +, -} with Raster objects (see examples). In that case you cannot specify an output filename. Moreover, the overlay function should be more efficient when using large data files that cannot be loaded into memory, as the use of the complex arithmetic functions might lead to the creation of many temporary files.

While you can supply functions such as \code{sum} or \code{mean}, it would be more direct to use the Raster* objects as arguments to those functions (e.g. \code{sum(r1,r2,r3)})  

See \code{\link{rasterize}} and \code{\link{extract}} for "overlays" involving Raster* objects and polygons, lines, or points.
}

\value{
Raster* object
}

\seealso{\code{ \link[raster]{calc},  \link[raster]{Arith-methods}} }

\examples{
r <- raster(ncol=10, nrow=10)
r1 <- init(r, fun=runif)
r2 <- init(r, fun=runif)
r3 <- overlay(r1, r2, fun=function(x,y){return(x+y)})

# long version for multiplication
r4 <- overlay(r1, r2, fun=function(x,y){(x*y)} )

#use the individual layers of a RasterStack to get a RasterLayer
s <- stack(r1, r2)
r5 <- overlay(s, fun=function(x,y) x*y )
# equivalent to
r5c <- calc(s, fun=function(x) x[1]*x[2] )


#Combine RasterStack and RasterLayer objects (s2 has four layers. 
# r1 (one layer) and s (two layers) are recycled) 
s2 <- stack(r1, r2, r3, r4)
b <- overlay(r1, s, s2, fun=function(x,y,z){return(x*y*z)} )

# use a single RasterLayer (same as calc function)
r6 <- overlay(r1, fun=sqrt)

# multiplication with more than two layers 
# (make sure the number of RasterLayers matches the arguments of 'fun')
r7 <- overlay(r1, r2, r3, r4, fun=function(a,b,c,d){return(a*b+c*d)} )  
# equivalent function, efficient if values can be loaded in memory
r8 <- r1 * r2 + r3 * r4

# Also works with multi-layer objects. 
s1 <- stack(r1, r2, r3)
x <- overlay(s1, s1, fun=function(x,y)x+y+5)

# in this case the first layer of the shorter object is recycled.
# i.e., s2 is treated as stack(r1, r3, r1)
s2 <- stack(r1, r3)
y <- overlay(s1, s2, fun=sum)
}
 
\keyword{methods}
\keyword{spatial}
