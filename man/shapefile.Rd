
\name{shapefile}

\alias{shapefile}
\alias{shapefile,character-method}
\alias{shapefile,Spatial-method}

\title{
Read or write a shapefile
}

\description{
Reading and writing of "ESRI shapefile" format spatial data. Only the three vector types (points, lines, and polygons) can be stored in shapefiles. 

A shapefile should consist of at least four files: .shp (the geometry), .dbf (the attributes), .shx (the index that links the two, and .prj (the coordinate reference system). If the .prj file is missing, a warning is given. If any other file is missing an error occurs (although one could in principle recover the .shx from the .shp file). Additional files are ignored.
}


\usage{
\S4method{shapefile}{character}(x, stringsAsFactors=FALSE, verbose=FALSE, warnPRJ=TRUE, ...)

\S4method{shapefile}{Spatial}(x, filename='', overwrite=FALSE, ...)
}
 
\arguments{
  \item{x}{character (a file name, when reading a shapefile) or Spatial* object (when writing a shapefile)}
  \item{filename}{character. Filename to write a shapefile}
  \item{overwrite}{logical. Overwrite existing shapefile?}
  \item{verbose}{logical. If \code{TRUE}, information about the file is printed}
  \item{warnPRJ}{logical. If \code{TRUE}, a warning is given if there is no .prj file}
  \item{stringsAsFactors}{logical. If \code{TRUE}, strings are converted to factors}
  \item{...}{Additional arguments passed to rgdal functions readOGR or writeOGR}
}

\value{
Spatial*DataFrame (reading). Nothing is returned when writing a shapefile.
}

\examples{
filename <- system.file("external/lux.shp", package="raster")
filename
p <- shapefile(filename)

\dontrun{
shapefile(p, 'copy.shp')
}
}


\keyword{spatial}

