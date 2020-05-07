\name{reclassify}

\docType{methods}

\alias{reclassify}
\alias{reclassify,Raster-method}

\title{Reclassify}

\description{
Reclassify values of a Raster* object. The function (re)classifies groups of values to other values. For example, all values between 1 and 10 become 1, and all values between 11 and 15 become 2 (see functions \code{\link{subs}} and \code{\link{cut}} for alternative approaches). 

Reclassification is done with matrix \code{rcl}, in the row order of the reclassify table. Thus, if there are overlapping ranges, the first time a number is within a range determines the reclassification value.  
}

\usage{
\S4method{reclassify}{Raster}(x, rcl, filename='', include.lowest=FALSE, right=TRUE, ...)
}

\arguments{
  \item{x}{Raster* object}
  
  \item{rcl}{matrix for reclassification. This matrix can have 3 or 2 columns. 
  
  In a \code{3-column matrix} the first two columns are "from" - "to" for the input values, and the third column "becomes" has the new value for that range. (You can also supply a vector that can be coerced into a n*3 matrix (with \code{byrow=TRUE})). 
  
  A \code{2-column matrix} represents ("is", "becomes") which can be useful for integer values. In that case, the \code{right} argument is automatically set to \code{NA}}

 \item{filename}{character. Output filename (optional) }
  
 \item{include.lowest}{logical, indicating if a value equal to the lowest value in rcl (or highest value in the second column, for right = FALSE) should be included. The default is \code{FALSE}}
 
 \item{right}{logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa. The default is \code{TRUE}. A special case is to use right=NA. In this case both the left and right intervals are open}

 \item{...}{additional arguments as for \code{\link{writeRaster}}}  
}

\value{
Raster* object
}

\seealso{ \code{ \link{subs}, \link{clamp}, \link{cut}, \link{calc}} } 




\examples{

r <- raster(ncols=36, nrows=18)
values(r) <- runif(ncell(r)) 
# reclassify the values into three groups 
# all values > 0 and <= 0.25 become 1, etc.
m <- c(0, 0.25, 1,  0.25, 0.5, 2,  0.5, 1, 3)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(r, rclmat)

# for values >= 0 (instead of > 0), do
rc <- reclassify(r, rclmat, include.lowest=TRUE)

# equivalent to
rc <- reclassify(r, c(-Inf,0.25,1, 0.25,0.5,2, 0.5,Inf,3))
}

\keyword{spatial}
