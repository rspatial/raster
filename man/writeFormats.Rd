\name{writeFormats}

\alias{writeFormats}

\title{File types for writing}

\description{
List supported file types for writing RasterLayer values to disk.

When a function writes a file to disk, the file format is determined by the 'format=' argument if supplied, or else by the file extension (if the extension is known). If other cases the default format is used. The 'factory-fresh' default format is 'raster', but this can be changed using \code{\link{rasterOptions}}.
}

\usage{
writeFormats()
}

\details{
writeFormats returns a matrix of the file formats (the "drivers") that are supported.

Supported formats include:


\tabular{llllr}{
	\tab \bold{File type} \tab \bold{Long name} \tab \bold{default extension} \tab \bold{Multiband support} \cr
    \tab \code{raster}    \tab 'Native' raster package format \tab .grd \tab Yes \cr
    \tab \code{ascii}     \tab ESRI Ascii  \tab .asc                 \tab  No \cr
    \tab \code{SAGA}      \tab SAGA GIS    \tab .sdat                \tab  No \cr
    \tab \code{IDRISI}    \tab IDRISI      \tab .rst                 \tab  No \cr
    \tab \code{CDF}       \tab netCDF (requires ncdf4) \tab .nc    \tab  Yes \cr
    \tab \code{GTiff}     \tab GeoTiff \tab .tif    \tab  Yes  \cr
    \tab \code{ENVI}      \tab ENVI .hdr Labelled       \tab .envi   \tab Yes  \cr
    \tab \code{EHdr}      \tab ESRI .hdr Labelled       \tab  .bil   \tab Yes  \cr
    \tab \code{HFA}       \tab Erdas Imagine Images (.img)  \tab .img \tab Yes  \cr
}
}



\examples{ 
writeFormats() 
}

\keyword{ spatial }
