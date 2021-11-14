\name{extend}

\alias{extend}
\alias{extend,Raster-method}
\alias{extend,Extent-method}


\title{Extend}

\description{
Extend returns an Raster* object with a larger spatial extent. The output Raster object has the outer minimum and maximum coordinates of the input Raster and Extent arguments. Thus, all of the cells of the original raster are included. See \code{\link[raster]{crop}} if you (also) want to remove rows or columns. 

There is also an extend method for Extent objects to enlarge (or reduce) an Extent. You can also use algebraic notation to do that (see examples).

This function has replaced function "expand" (to avoid a name conflict with the Matrix package).
}

\usage{
\S4method{extend}{Raster}(x, y, value=NA, snap="near", filename='', ...) 

\S4method{extend}{Extent}(x, y, ...) 
}

\arguments{
  \item{x}{Raster or Extent object}
  \item{y}{If \code{x} is a Raster object, \code{y} should be an Extent object, or any object that is or has an Extent object, or an object from which it can be extracted (such as sp objects). Alternatively, you can provide a numeric vector of length 2 indicating the number of rows and columns that need to be added (or a single number when the number of rows and columns is equal)
  
  If \code{x} is an Extent object, \code{y} should be a numeric vector of 1, 2, or 4 elements}
  \item{value}{value to assign to new cells}  
  \item{snap}{Character. One of "near", "in", or "out", to determine in which direction the extent should be aligned. To the nearest border, inwards or outwards}  
  \item{filename}{Character (optional)}  
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}  

}

\value{
RasterLayer or RasterBrick, or Extent
}


\author{Robert J. Hijmans and Etienne B. Racine (Extent method)}

\seealso{\code{\link[raster]{crop}}, \code{\link[raster]{merge}}}

\examples{
r <- raster(xmn=-150, xmx=-120, ymx=60, ymn=30, ncol=36, nrow=18)
values(r) <- 1:ncell(r)
e <- extent(-180, 0, 0, 90)
re <- extend(r, e)

# extend with a number of rows and columns (at each side)
re2 <- extend(r, c(2,10))

# Extent object
e <- extent(r)
e
extend(e, 10)
extend(e, 10, -10, 0, 20)
e + 10
e * 2
}

\keyword{spatial}
