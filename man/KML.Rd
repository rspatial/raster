\name{KML}

\alias{KML}
\alias{KML,Spatial-method}
\alias{KML,RasterLayer-method}
\alias{KML,RasterStackBrick-method}

\title{Write a KML or KMZ file}

\description{
Export raster data to a KML file and an accompanying PNG image file. Multi-layer objects can be used to create an animation. 
The function attempts to combine these into a single (and hence more convenient) KMZ file (a zip file containing the KML and PNG files). 

See package plotKML for more advanced functionality
}

\usage{
\S4method{KML}{RasterLayer}(x, filename, col=rev(terrain.colors(255)), 
     colNA=NA, maxpixels=100000, blur=1, zip='', overwrite=FALSE, ...)

\S4method{KML}{RasterStackBrick}(x, filename, time=NULL, col=rev(terrain.colors(255)), 
     colNA=NA, maxpixels=100000, blur=1, zip='', overwrite=FALSE, ...)

\S4method{KML}{Spatial}(x, filename, zip='', overwrite=FALSE, ...)
}

\arguments{
\item{x}{Raster* object}
\item{filename}{output filename}
\item{time}{character vector with time lables for multilayer objects. The length of this vector should be nlayers(x) to indicate "when" or nlayers(x)+1 to indicate "begin-end"}
\item{col}{color scheme to be used (see \link[graphics]{image})}
\item{colNA}{The color to use for the background (default is transparent)}
\item{maxpixels}{maximum number of pixels. If ncell(raster) > maxpixels, sampleRegular is used to reduce the number of pixels}
\item{blur}{Integer (default=1). Higher values help avoid blurring of isolated pixels (at the expense of a png file that is blur^2 times larger)}
\item{zip}{If there is no zip program on your path (on windows), you can supply the full path to a zip.exe here, in order to make a KMZ file}
\item{overwrite}{logical. If \code{TRUE}, overwrite the file if it exists}
\item{...}{If \code{x} is a Raster* object, additional arguments that can be passed to \link[graphics]{image}}
}


\value{
None. Used for the side-effect files written to disk. 
}

\author{This function was adapted for the raster package by Robert J. Hijmans, with ideas from Tony Fischbach, and based on functions in the maptools package by Duncan Golicher, David Forrest and Roger Bivand.}


\examples{
\dontrun{
# Meuse data from the sp package
data(meuse.grid)
b <- rasterFromXYZ(meuse.grid)
projection(b) <- "+init=epsg:28992" 				  
# transform to longitude/latitude
p <- projectRaster(b, crs="+proj=longlat +datum=WGS84", method='ngb')
KML(p, file='meuse.kml')
}
}

\keyword{spatial}
