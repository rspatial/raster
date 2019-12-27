\name{brick}

\docType{methods}

\alias{brick}

\alias{brick,character-method}
\alias{brick,missing-method}
\alias{brick,RasterLayer-method}
\alias{brick,RasterStack-method}
\alias{brick,RasterBrick-method}
\alias{brick,Extent-method}
\alias{brick,array-method}
\alias{brick,list-method}
\alias{brick,SpatialPixels-method}
\alias{brick,SpatialGrid-method}
\alias{brick,kasc-method}
\alias{brick,grf-method}
\alias{brick,SpatRaster-method}

\title{ Create a RasterBrick object}

\description{
A RasterBrick is a multi-layer raster object. They are typically created from a multi-layer (band) file; but they can also exist entirely in memory. They are similar to a RasterStack (that can be created with \code{\link[raster]{stack}}), but processing time should be shorter when using a RasterBrick. Yet they are less flexible as they can only point to a single file.

A RasterBrick can be created from RasterLayer objects, from a RasterStack, or from a (multi-layer) file. The can also be created from SpatialPixels*, SpatialGrid*, and Extent objects, and from a three-dimensional array.
}

\usage{
\S4method{brick}{character}(x, ...)

\S4method{brick}{RasterStack}(x, values=TRUE, nl, filename='', ...) 

\S4method{brick}{RasterBrick}(x, nl, ...)

\S4method{brick}{RasterLayer}(x, ..., values=TRUE, nl=1, filename='') 

\S4method{brick}{missing}(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, nl=1, crs)

\S4method{brick}{Extent}(x, nrows=10, ncols=10, crs=NA, nl=1)

\S4method{brick}{array}(x, xmn=0, xmx=1, ymn=0, ymx=1, crs=NA, transpose=FALSE)

\S4method{brick}{SpatialGrid}(x)

\S4method{brick}{SpatialPixels}(x)
}


\arguments{
\item{x}{character (filename, see Details); Raster* object; missing; array; SpatialGrid*; SpatialPixels*; Extent; or list of Raster* objects. Supported file types are the 'native' raster package format and those that can be read via rgdal (see \code{\link[rgdal]{readGDAL}}), and NetCDF files (see details)}
\item{...}{see Details}
\item{values}{logical. If \code{TRUE}, the cell values of '\code{x}' are copied to the RasterBrick object that is returned}
\item{nl}{integer > 0. How many layers should the RasterBrick have?}
\item{filename}{character. Filename if you want the RasterBrick to be saved on disk}
\item{nrows}{integer > 0. Number of rows}
\item{ncols}{integer > 0. Number of columns}
\item{xmn}{minimum x coordinate (left border)}
\item{xmx}{maximum x coordinate (right border)}
\item{ymn}{minimum y coordinate (bottom border)}
\item{ymx}{maximum y coordinate (top border)}
\item{crs}{character or object of class CRS. PROJ4 type description of a Coordinate Reference System (map projection). If this argument is missing, and the x coordinates are within -360 .. 360 and the y coordinates are within -90 .. 90, "+proj=longlat +datum=WGS84" is used}
\item{transpose}{if \code{TRUE}, the values in the array are transposed}
}

\details{
If \code{x} is a RasterLayer, the additional arguments can be used to pass additional Raster* objects.

If there is a \code{filename} argument, the additional arguments are as for \code{\link{writeRaster}}. 

If \code{x} represents a filename there is the following additional argument:

\code{native}: logical. If \code{TRUE} (not the default), reading and writing of IDRISI, BIL, BSQ, BIP, and Arc ASCII files is done with native (raster package) drivers, rather then via rgdal.

In addition, if \code{x} is a \bold{NetCDF} filename there are the following additional arguments:
 
\code{varname}: character. The variable name (e.g. 'altitude' or 'precipitation'. If not supplied and the file has multiple
variables are a guess will be made (and reported))

\code{lvar}: integer > 0 (default=3). To select the 'level variable' (3rd dimension variable) to use, if the file has 4 dimensions (e.g. depth instead of time)

\code{level}: integer > 0 (default=1). To select the 'level' (4th dimension variable) to use, if the file has 4 dimensions, e.g. to create a RasterBrick of weather over time at a certain height.

\code{dims}: integer vector to indicated the order of the dimensions. Default is \code{dims=c(1,2,3)} (rows, cols, time).  

To use NetCDF files the \code{ncdf4} package needs to be available. It is assumed that these files follow, or are compatible with the CF-1 convention.
}

\value{
RasterBrick
}

\seealso{ \code{\link[raster]{raster}} }

\examples{
b <- brick(system.file("external/rlogo.grd", package="raster"))
b
nlayers(b)
names(b)
extract(b, 870)
}

\keyword{methods}
\keyword{spatial}
