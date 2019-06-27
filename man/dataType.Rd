\name{dataType}

\alias{dataType}
\alias{dataType<-}

\title{Data type }

\description{
Get the datatype of a RasterLayer object. The datatype determines the interpretation of values written to disk. 
Changing the datatype of a Raster* object does not directly affect the way they are stored in memory. For native file formats (.grd/.gri files) it does affect how values are read from file. This is not the case for file formats that are read via rgdal (such as .tif and .img files) or netcdf.

If you change the datatype of a RasterLayer and then read values from a native format file these may be completely wrong, so only do this for debugging or when the information in the header file was wrong. To set the datatype of a new file, you can give a 'datatype' argument to the functions that write values to disk (e.g. \code{\link{writeRaster}}). 
}

\usage{
dataType(x)
dataType(x) <- value
}

\arguments{
  \item{x}{ A \code{RasterLayer} object }
  \item{value}{ A data type (see below) }
}

\details{
Setting the data type is useful if you want to write values to disk. In other cases use functions such as round()

Datatypes are described by 5 characters. The first three indicate whether the values are integers, decimal number or logical values. The fourth character indicates the number of bytes used to save the values on disk, and the last character indicates whether the numbers are signed (i.e. can be negative and positive values) or not (only zero and positive values allowed)

The following datatypes are available:

\tabular{lll}{
\bold{Datatype definition} \tab \bold{minimum possible value} \tab \bold{maximum possible value} \cr
\code{LOG1S} \tab FALSE (0)\tab TRUE (1) \cr
\code{INT1S} \tab -127 \tab  127 \cr
\code{INT1U} \tab 0 \tab  255 \cr
\code{INT2S} \tab -32,767\tab  32,767 \cr
\code{INT2U} \tab 0 \tab  65,534 \cr
\code{INT4S} \tab -2,147,483,647 \tab 2,147,483,647 \cr
\code{INT4U} \tab 0 \tab 4,294,967,296 \cr
\code{FLT4S} \tab -3.4e+38 \tab  3.4e+38 \cr
\code{FLT8S} \tab -1.7e+308 \tab   1.7e+308 \cr
}

For all integer types, except the single byte types, the lowest (signed) or highest (unsigned) value is used to store \code{NA}. Single byte files do not have \code{NA} values. Logical values are stored as signed single byte integers, they do have an \code{NA} value (-127)

\code{INT4U} is available but they are best avoided as R does not support 32-bit unsigned integers.
}

\value{
Raster* object
}



\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
dataType(r)
\dontrun{
s <- writeRaster(r, 'new.grd', datatype='INT2U', overwrite=TRUE)
dataType(s)
}
}

\keyword{ spatial }

