\name{calc}

\docType{methods}

\alias{calc}
\alias{calc,Raster,function-method}

\title{Calculate}

\description{ 
Calculate values for a new Raster* object from another Raster* object, using a formula. 


If \code{x} is a RasterLayer, \code{fun} is typically a function that can take a single vector as input, and return a vector of values of the same length (e.g. \code{sqrt}). If \code{x} is a RasterStack or RasterBrick, fun should operate on a vector of values (one vector for each cell). \code{calc} returns a RasterLayer if \code{fun} returns a single value (e.g. \code{sum}) and it returns a RasterBrick if \code{fun} returns more than one number, e.g., \code{fun=quantile}.
 
In many cases, what can be achieved with \code{calc}, can also be accomplished with a more intuitive 'raster-algebra' notation (see \code{\link[raster]{Arith-methods}}). For example, \code{r <- r * 2} instead of

 
\code{r <- calc(r, fun=function(x){x * 2}}, or \code{r <- sum(s)} instead of


\code{r <- calc(s, fun=sum)}. However, \code{calc} should be faster when using complex formulas on large datasets. With \code{calc} it is possible to set an output filename and file type preferences. 
 
See (\code{\link[raster]{overlay}}) to use functions that refer to specific layers, like (\code{function(a,b,c){a + sqrt(b) / c}})
}

\usage{
\S4method{calc}{Raster,function}(x, fun, filename='', na.rm, forcefun=FALSE, forceapply=FALSE, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{fun}{function}
  \item{filename}{character. Output filename (optional)}
  \item{na.rm}{Remove \code{NA} values, if supported by 'fun' (only relevant when summarizing a multilayer Raster object into a RasterLayer)}
  \item{forcefun}{logical. Force \code{calc} to not use fun with apply; for use with ambiguous functions and for debugging (see Details)}
  \item{forceapply}{logical. Force \code{calc} to use fun with apply; for use with ambiguous functions and for debugging (see Details)}
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}    
}

\value{
a Raster* object
}

\details{
The intent of some functions can be ambiguous. Consider:

\code{library(raster)}

\code{r <- raster(volcano)}

\code{calc(r, function(x) x * 1:10)}

In this case, the cell values are multiplied in a vectorized manner and a single layer is returned where the first cell has been multiplied with one, the second cell with two, the 11th cell with one again, and so on. But perhaps the intent was to create 10 new layers (\code{x*1, x*2, ...})? This can be achieved by using argument \code{forceapply=TRUE} 


\code{calc(r, function(x) x * 1:10, forceapply=TRUE)}

}



\note{
For large objects \code{calc} will compute values chunk by chunk. This means that for the result of \code{fun} to be  correct it should not depend on having access to _all_ values at once. For example, to scale the values of a Raster* object by subtracting its mean value (for each layer), you would _not_ do, for Raster object \code{x}: 

\code{calc(x, function(x)scale(x, scale=FALSE))}

Because the mean value of each chunk will likely be different. Rather do something like

\code{m <- cellStats(x, 'mean')}

\code{x - m}
}


\seealso{ \code{ \link[raster]{overlay}} ,  \code{ \link[raster]{reclassify}},  \link[raster]{Arith-methods}, \link[raster]{Math-methods}} 


\author{Robert J. Hijmans and Matteo Mattiuzzi}

\examples{
r <- raster(ncols=36, nrows=18)
values(r) <- 1:ncell(r)

# multiply values with 10
fun <- function(x) { x * 10 }
rc1 <- calc(r, fun)

# set values below 100 to NA. 
fun <- function(x) { x[x<100] <- NA; return(x) }
rc2 <- calc(r, fun)

# set NA values to -9999
fun <- function(x) { x[is.na(x)] <- -9999; return(x)} 
rc3 <- calc(rc2, fun)

# using a RasterStack as input
s <- stack(r, r*2, sqrt(r))
# return a RasterLayer
rs1 <- calc(s, sum)

# return a RasterBrick
rs2 <- calc(s, fun=function(x){x * 10})
# recycling by layer
rs3 <- calc(s, fun=function(x){x * c(1, 5, 10)})

# use overlay when you want to refer to individual layer in the function
# but it can be done with calc: 
rs4 <- calc(s, fun=function(x){x[1]+x[2]*x[3]})

## 
# Some regression examples
## 

# create data
r <- raster(nrow=10, ncol=10)
s1 <- lapply(1:12, function(i) setValues(r, rnorm(ncell(r), i, 3)))
s2 <- lapply(1:12, function(i) setValues(r, rnorm(ncell(r), i, 3)))
s1 <- stack(s1)
s2 <- stack(s2)

# regression of values in one brick (or stack) with another
s <- stack(s1, s2)
# s1 and s2 have 12 layers; coefficients[2] is the slope
fun <- function(x) { lm(x[1:12] ~ x[13:24])$coefficients[2] }
x1 <- calc(s, fun)

# regression of values in one brick (or stack) with 'time'
time <- 1:nlayers(s)
fun <- function(x) { lm(x ~ time)$coefficients[2] }
x2 <- calc(s, fun)

# get multiple layers, e.g. the slope _and_ intercept
fun <- function(x) { lm(x ~ time)$coefficients }
x3 <- calc(s, fun)


### A much (> 100 times) faster approach is to directly use 
### linear algebra and pre-compute some constants

## add 1 for a model with an intercept
X <- cbind(1, time)

## pre-computing constant part of least squares
invXtX <- solve(t(X) \%*\% X) \%*\% t(X)

## much reduced regression model; [2] is to get the slope
quickfun <- function(y) (invXtX \%*\% y)[2]
x4 <- calc(s, quickfun) 
}


\keyword{methods}
\keyword{spatial}
