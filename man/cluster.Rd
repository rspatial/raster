\name{cluster}

\alias{beginCluster}
\alias{endCluster}
\alias{clusterR}

\title{Use a multi-core cluster}

\description{
\code{beginCluster} creates, and \code{endCluster} deletes a 'snow' cluster object. This object can be used for multi-core computing with those 'raster' functions that support it. 

\code{beginCluster} determines the number of nodes (cores) that are available and uses all of them (unless the argument \code{n} is used). 

NOTE: beginCluster may fail when the package 'nws' is installed. You can fix that by removing the 'nws' package, or by setting the cluster type manually, e.g. \code{beginCluster(type="SOCK")}

endCluster closes the cluster and removes the object.

The use of the cluster is automatic in these functions: \code{\link{projectRaster}}, \code{\link{resample}} and in \code{\link{extract}} when using polygons.

\code{clusterR} is a flexible interface for using cluster with other functions. This function only works with functions that have a Raster* object as first argument and that operate on a cell by cell basis (i.e., there is no effect of neigboring cells) and return an object with the same number of cells as the input raster object. The first argument of the function called must be a Raster* object. There can only be one Raster* object argument. For example, it works with \code{\link{calc}} and it also works with \code{\link{overlay}} as long as you provide a single RasterStack or RasterBrick as the first argument. 

This function is particularly useful to speed up computations in functions like predict, interpolate, and perhaps calc. 

Among other functions, it does _not_ work with merge, crop, mosaic, (dis)aggregate, resample, projectRaster, focal, distance, buffer, direction. But note that projectRaster has a build-in capacity for clustering that is automatically used if beginCluster() has been called.
}

\usage{
beginCluster(n, type='SOCK', nice, exclude)
endCluster()
clusterR(x, fun, args=NULL, export=NULL, filename='', cl=NULL, m=2, ...)
}

\arguments{
\item{n}{Integer. The number of nodes to be used (optional)}
\item{type}{Character. The cluster type to be used}
\item{nice}{Integer. To set the prioirty for the workers, between -20 and 20 (UNIX like platforms only)}
\item{exclude}{Character. Packages to exclude from loading on the nodes (because they may fail there) but are required/loaded on the master }
\item{x}{Raster* object}
\item{fun}{function that takes \code{x} as its first argument}
\item{args}{list with the arguments for the function (excluding \code{x}, which should always be the first argument}
\item{export}{character. Vector of variable names to export to the cluster nodes such that the are visible to fun (e.g. a parameter that is not passed as an argument)}
\item{filename}{character. Output filename (optional)}
\item{cl}{cluster object (do not use it if beginCluster() has been called}
\item{m}{tuning parameter to determine how many blocks should be used. The number is rounded and multiplied with the number of nodes.}
\item{...}{additional arguments as for \code{\link{writeRaster}}}
}

\note{
If you want to write your own cluster-enabled functions see \code{\link{getCluster}, \link{returnCluster}}, and the vignette about writing functions.
}

\value{
beginCluster and endCluster: None. The side effect is to create or delete a cluster object.

clusterR: as for the function called with argument \code{fun}
}

\examples{

\dontrun{
# set up the cluster object for parallel computing
beginCluster()

r <- raster()
r[] <- 1:ncell(r)

x <- clusterR(r, sqrt, verbose=T)

f1 <- function(x) calc(x, sqrt)
y <- clusterR(r, f1)

s <- stack(r, r*2, r*3)
f2 <- function(d,e,f) (d + e) / (f * param)
param <- 122
ov <- clusterR(s, overlay, args=list(fun=f2), export='param')

pts <- matrix(c(0,0, 45,45), ncol=2, byrow=T)
d <- clusterR(r, distanceFromPoints, args=list(xy=pts))

values(r) <- runif(ncell(r))
m <- c(0, 0.25, 1,  0.25, 0.5, 2,  0.5, 1, 3)
m <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- clusterR(r, reclassify, args=list(rcl=m, right=FALSE), 
               filename='rcltest.grd', datatype='INT2S', overwrite=TRUE)

# equivalent to:
rc2 <- reclassify(r, rcl=m, right=FALSE, filename='rcltest.grd', datatype='INT2S', overwrite=TRUE)


# example with the calc function
a <- 10
f3 <- function(x) sum(x)+a

z1 <- clusterR(s, calc, args=list(fun=f3), export='a')

# for some raster functions that use another function as an argument 
# you can write your own parallel function instead of using clusterR
# get cluster object created with beginCluster
cl <- getCluster()  

library(parallel)
clusterExport(cl, "a")
z2 <- calc(s, fun=function(x){ parApply(cl, x, 1, f3)} )
# set flag that cluster is available again
returnCluster()
#

# done with cluster object		
endCluster()
}
}

\author{Matteo Mattiuzzi and Robert J. Hijmans}

\keyword{ spatial }

