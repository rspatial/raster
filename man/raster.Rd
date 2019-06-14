\name{raster}

\docType{methods}

\alias{raster}
\alias{raster,missing-method}
\alias{raster,character-method}
\alias{raster,Extent-method}
\alias{raster,BasicRaster-method}
\alias{raster,RasterLayer-method}
\alias{raster,RasterLayerSparse-method}
\alias{raster,RasterStack-method}
\alias{raster,RasterBrick-method}
\alias{raster,Spatial-method}
\alias{raster,SpatialGrid-method}
\alias{raster,SpatialPixels-method}
\alias{raster,matrix-method}
\alias{raster,big.matrix-method}
\alias{raster,list-method}
\alias{raster,im-method}
\alias{raster,asc-method}
\alias{raster,kasc-method}
\alias{raster,kde-method}
\alias{raster,grf-method}
\alias{raster,sf-method}
\alias{raster,GridTopology-method}
\alias{raster,SpatRaster-method}



\title{Create a RasterLayer object}

\description{
Methods to create a RasterLayer object. RasterLayer objects can be created from scratch, a file, an Extent object, a matrix, an 'image' object, or from a Raster*, Spatial*, im (spatstat) asc, kasc (adehabitat*), grf (geoR) or kde object.

In many cases, e.g. when a RasterLayer is created from a file, it does (initially) not contain any cell (pixel) values in (RAM) memory, it only has the parameters that describe the RasterLayer. You can access cell-values with \code{\link[raster]{getValues}, \link[raster]{extract}} and related functions. You can assign new values with  \code{\link[raster]{setValues}} and with \code{\link[raster]{replacement}}.

For an overview of the functions in the raster package have a look here: \code{\link{raster-package}}.
}


\usage{
\S4method{raster}{character}(x, band=1, ...)

\S4method{raster}{RasterLayer}(x) 

\S4method{raster}{RasterStack}(x, layer=0) 

\S4method{raster}{RasterBrick}(x, layer=0) 

\S4method{raster}{missing}(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, 
		crs, ext, resolution, vals=NULL)

\S4method{raster}{Extent}(x, nrows=10, ncols=10, crs=NA, ...)

\S4method{raster}{matrix}(x, xmn=0, xmx=1, ymn=0, ymx=1, crs=NA, template=NULL)

\S4method{raster}{big.matrix}(x, xmn=0, xmx=1, ymn=0, ymx=1, crs=NA, template=NULL)

\S4method{raster}{Spatial}(x, origin, ...) 

\S4method{raster}{SpatialGrid}(x, layer=1, values=TRUE)

\S4method{raster}{SpatialPixels}(x, layer=1, values=TRUE)

\S4method{raster}{sf}(x, origin, ...) 
}

\arguments{
\item{x}{filename (character), Extent, Raster*, sf, SpatialPixels*, SpatialGrid*, object, 'image', matrix, im, or missing. Supported file types are the 'native' raster package format and those that can be read via \code{rgdal} (see \code{\link[rgdal]{readGDAL}}}
\item{band}{integer. The layer to use in a multi-layer file}
\item{...}{Additional arguments, see Details }  
\item{layer}{integer. The layer (variable) to use in a multi-layer file, or the layer to extract from a RasterStack/Brick or SpatialPixelsDataFrame or SpatialGridDataFrame. An empty RasterLayer (no associated values) is returned if \code{layer=0}}
\item{values}{logical. If \code{TRUE}, the cell values of '\code{x}' are copied to the RasterLayer object that is returned}
\item{nrows}{integer > 0. Number of rows}
\item{ncols}{integer > 0. Number of columns}
\item{xmn}{minimum x coordinate (left border)}
\item{xmx}{maximum x coordinate (right border)}
\item{ymn}{minimum y coordinate (bottom border)}
\item{ymx}{maximum y coordinate (top border)}
\item{ext}{object of class Extent. If present, the arguments xmn, xmx, ymn and ynx are ignored}
\item{crs}{character or object of class CRS. PROJ.4 type description of a Coordinate Reference System (map projection). If this argument is missing, and the x coordinates are within -360 .. 360 and the y coordinates are within -90 .. 90, "+proj=longlat +datum=WGS84" is used. Also see under Details if \code{x} is a character (filename)}
\item{resolution}{numeric vector of length 1 or 2 to set the resolution (see \code{\link{res}}). If this argument is used, arguments \code{ncols} and \code{nrows} are ignored }
\item{vals}{optional. Values for the new RasterLayer. Accepted formats are as for \code{\link{setValues}}}
\item{origin}{minimum y coordinate (bottom border)}
\item{template}{Raster* or Extent object used to set the extent (and CRS in case of a Raster* object). If not \code{NULL}, arguments \code{xmn}, \code{xmx}, \code{ymn}, \code{ymx} and \code{crs} (unless \code{template} is an Extent object) are ignored}
}

\details{
If \code{x} is a filename, the following additional variables are recognized:

\code{sub}: positive integer. Subdataset number for a file with subdatasets

\code{native}: logical.  Default is \code{FALSE} except when package \code{rgdal} is missing. If \code{TRUE}, reading and writing of IDRISI, BIL, BSQ, BIP, SAGA, and Arc ASCII files is done with native (raster package) drivers, rather then via rgdal. 'raster' and netcdf format files are always read with native drivers. 

\code{RAT}: logical. The default is \code{TRUE}, in which case a raster attribute table is created for files that have one

\code{offset}: integer. To indicate the number of header rows on non-standard ascii files (rarely useful; use with caution)

\code{crs}: character. PROJ.4 string to set the CRS. Ignored when the file provides a CRS description that can be interpreted.


If \code{x} represents a \bold{NetCDF} file, the following additional variable is recognized:

\code{varname}: character. The variable name, such as 'tasmax' or 'pr'. If not supplied and the file has multiple variables are a guess will be made (and reported)

\code{lvar}: integer > 0 (default=3). To select the 'level variable' (3rd dimension variable) to use, if the file has 4 dimensions (e.g. depth instead of time)\cr

\code{level}: integer > 0 (default=1). To select the 'level' (4th dimension variable) to use, if the file has 4 dimensions, e.g. to create a RasterBrick of weather over time at a certain height. \cr

To use NetCDF files the \code{ncdf4} package needs to be available. It is assumed that these files follow, or are compatible with, the CF-1 convention (The GMT format may also work). If the ncdf file does not have a standard extension (which is used to recognize the file format), you can use argument \code{ncdf=TRUE} to indicate the format.


If \code{x} is a \code{Spatial} or an \code{Extent} object, additional arguments are for the method with signature \code{'missing'}

}

\value{
RasterLayer
}

\seealso{ \code{\link[raster]{stack}, \link[raster]{brick}} }

\examples{
# Create a RasterLayer object from a file
#   N.B.: For your own files, omit the 'system.file' and 'package="raster"' bits
#   these are just to get the path to files installed with the package

f <- system.file("external/test.grd", package="raster")
f
r <- raster(f)

logo <- raster(system.file("external/rlogo.grd", package="raster")) 


#from scratch
r1 <- raster(nrows=108, ncols=21, xmn=0, xmx=10)

#from an Extent object
e <- extent(r)
r2 <- raster(e)

#from another Raster* object
r3 <- raster(r)
s <- stack(r, r, r)
r4 <- raster(s)
r5 <- raster(s, 3)


\dontrun{
# from NSIDC sea ice concentration file
baseurl <- "ftp://sidads.colorado.edu/pub/DATASETS/"
# southern hemisphere
f1 <- paste(baseurl, 
 "nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/2013/nt_20130114_f17_v01_s.bin",
 sep='')
# or northern hemisphere
f2 <- paste(baseurl, 
 "nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/daily/2013/nt_20130105_f17_v01_n.bin",
 sep='')
     
if (!file.exists(basename(f1))) download.file(f1, basename(f1), mode = "wb")
ice1 <- raster(basename(f1))

if (!file.exists(basename(f2))) download.file(f2, basename(f2), mode = "wb")
ice2 <- raster(basename(f2))
     
}
}

\keyword{methods}
\keyword{spatial}

