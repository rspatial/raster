\name{writeRaster}

\alias{writeRaster,RasterLayer,character-method}
\alias{writeRaster,RasterStackBrick,character-method}

\alias{writeRaster}

\title{Write raster data to a file}

\description{
Write an entire Raster* object to a file, using one of the many supported formats. See \code{\link[raster]{writeValues}} for writing in chunks (e.g.  by row). 

When writing a file to disk, the file format is determined by the 'format=' argument if supplied, or else by the file extension (if the extension is known). If other cases the default format is used. The default format is 'raster', but this setting can be changed (see \code{\link{rasterOptions}}).
}

\usage{
\S4method{writeRaster}{RasterLayer,character}(x, filename, format, ...)

\S4method{writeRaster}{RasterStackBrick,character}(x, filename, format, bylayer, suffix='numbers', ...)
}

\arguments{
\item{x}{Raster* object}
\item{filename}{Output filename}
\item{format}{Character. Output file type. See \code{\link[raster]{writeFormats}}. If this argument is not provided, it is attempted to infer it from the filename extension. If that fails, the default format is used. The default format is 'raster', but this can be changed using \code{\link{rasterOptions}}}
\item{...}{Additional arguments: 
  		
\code{datatype}{Character. Output data type (e.g. 'INT2S' or 'FLT4S'). 
See \code{\link{dataType}}. If no datatype is specified, 'FLT4S' is used,
unless this default value was changed with \code{\link{rasterOptions}}}
	
\code{overwrite}: Logical. If TRUE, "filename" will be overwritten if it exists
	
\code{progress}: Character. Set a value to show a progress bar. Valid values are "text" and "window".

\code{NAflag}: Numeric. To overwrite the default value used to represent \code{NA} in a file 
	
\code{bandorder}: Character. 'BIL', 'BIP', or 'BSQ'. For 'native' file formats only.
For some other formats you can use the 'options' argument (see below)
	
\code{options}: Character. File format specific GDAL options. E.g., when
writing a geotiff file you can use: \code{options=c("COMPRESS=NONE", "TFW=YES")}

You can use options=c("PROFILE=BASELINE") to create a plain  tif with no GeoTIFF tags. 
This can be useful when writing files to be read by applications intolerant of unrecognised tags. 
(see \url{http://www.gdal.org/frmt_gtiff.html})

NetCDF files have the following additional, optional, arguments: \code{varname}, \code{varunit}, \code{longname}, \code{xname}, \code{yname}, \code{zname}, \code{zunit}

\code{prj}: Logical. If \code{TRUE}, the crs is written to a .prj file. This can be useful 
when writing to an ascii file or another file type that does not store the crs
}

\item{bylayer}{if \code{TRUE}, write a separate file for each layer. You can provide a vector of filenames that matches the number of layers. Or you can provide a single filename that will get a unique suffix (see below)}

\item{suffix}{'numbers' or 'names' to determine the suffix that each file gets when \code{bylayer=TRUE}; either a number between \code{1} and \code{nlayers(x)} or \code{names(x)}}
}


\details{
See \code{writeFormats} for supported file types ("formats", "drivers").

The rgdal package is needed, except for these file formats: 'raster', 'BIL', 'BIP', 'BSQ', 'SAGA', 'ascii', 'IDRISI', and 'CDF'. Some of these formats can be used with or without rgdal (idrisi, SAGA, ascii).  You need the 'ncdf4' library for the 'CDF' format. 

In multi-layer files (i.e. files saved from RasterStack or RasterBrick objects), in the native 'raster' format, the band-order can be set to BIL ('Bands Interleaved by Line'), BIP ('Bands Interleaved by Pixels') or BSQ ('Bands SeQuential'). Note that bandorder is not the same as filetype here. 

Supported file types include:

\tabular{llllr}{
	\tab \bold{File type} \tab \bold{Long name} \tab \bold{default extension} \tab \bold{Multiband support} \cr
    \tab \code{raster}    \tab 'Native' raster package format \tab .grd \tab Yes \cr
    \tab \code{ascii}     \tab ESRI Ascii  \tab .asc                 \tab  No \cr
    \tab \code{SAGA}      \tab SAGA GIS    \tab .sdat                \tab  No \cr
    \tab \code{IDRISI}    \tab IDRISI      \tab .rst                 \tab  No \cr
    \tab \code{CDF}       \tab netCDF (requires ncdf4) \tab .nc    \tab  Yes \cr
    \tab \code{GTiff}     \tab GeoTiff (requires rgdal) \tab .tif    \tab  Yes  \cr
    \tab \code{ENVI}      \tab ENVI .hdr Labelled       \tab .envi   \tab Yes  \cr
    \tab \code{EHdr}      \tab ESRI .hdr Labelled       \tab  .bil   \tab Yes  \cr
    \tab \code{HFA}       \tab Erdas Imagine Images (.img)  \tab .img \tab Yes  \cr
}
}

\value{
This function is used for the side-effect of writing values to a file.
}



\seealso{\code{\link[raster]{writeFormats}}, \code{\link[raster]{writeValues}} }  

\examples{ 
tmp <- tempdir()
r <- raster(system.file("external/test.grd", package="raster"))

# take a small part
r <- crop(r, extent(179880, 180800, 329880, 330840) )

# write to an integer binary file
rf <- writeRaster(r, filename=file.path(tmp, "allint.grd"), datatype='INT4S', overwrite=TRUE)

# make a brick and save multi-layer file
b <- brick(r, sqrt(r))
bf <- writeRaster(b, filename=file.path(tmp, "multi.grd"), bandorder='BIL', overwrite=TRUE)

# write to a new geotiff file (depends on rgdal)
if (require(rgdal)) {
  rf <- writeRaster(r, filename=file.path(tmp, "test.tif"), format="GTiff", overwrite=TRUE)
  bf <- writeRaster(b, filename=file.path(tmp, "multi.tif"), 
						options="INTERLEAVE=BAND", overwrite=TRUE)
}
 
# write to netcdf 
if (require(ncdf4)) {	
 rnc <- writeRaster(r, filename=file.path(tmp, "netCDF.nc"), format="CDF", overwrite=TRUE)   
}
}


\keyword{ spatial }
\keyword{ methods }

