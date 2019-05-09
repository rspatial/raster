\name{rasterFromXYZ}

\alias{rasterFromXYZ}

\title{ Create a Raster* object from x, y, z values }

\description{
Create a Raster* object from x, y and z values. x and y represent spatial coordinates and must be on a regular grid. If the resolution is not supplied, it is assumed to be the minimum distance between x and y coordinates, but a resolution of up to 10 times smaller is evaluated if a regular grid can otherwise not be created. z values can be single or multiple columns (variables)
If the exact properties of the RasterLayer are known beforehand, it may be preferable to simply create a new RasterLayer with the raster function instead, compute cell numbers and assign the values with these (see example below).
}

\usage{
rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
}

\arguments{
\item{xyz}{matrix or data.frame with at least three columns: x and y coordinates, and values (z). There may be several 'z' variables (columns)}
\item{res}{numeric. The x and y cell resolution (optional)}
\item{crs}{CRS object or a character string describing a projection and datum in PROJ.4 format}
\item{digits}{numeric, indicating the requested precision for detecting whether points are on a regular grid (a low number of digits is a low precision)}
}

\value{
RasterLayer or RasterBrick
}

\seealso{See \code{\link{rasterize} for points that are not on a regular grid} }



\examples{
r <- raster(nrow=5, ncol=5, xmn=0, xmx=10, ymn=0, ymx=10, crs=NA)
set.seed(1)
values(r) <- sample(1:25)
r[r < 15] <- NA
xyz <- rasterToPoints(r)

rst <- rasterFromXYZ(xyz)

# equivalent to:
rr <- raster(nrow=5, ncol=5, xmn=0, xmx=10, ymn=0, ymx=10)
cells <- cellFromXY(rr, xyz[,1:2])
rr[cells] <- xyz[,3]

# multiple layers
xyzz <- cbind(xyz, a=1:nrow(xyz), b=nrow(xyz):1)
b <- rasterFromXYZ(xyzz)
}

\keyword{methods}
\keyword{spatial}
