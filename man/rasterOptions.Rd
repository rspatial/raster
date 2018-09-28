\name{Options}

\alias{rasterOptions}
\alias{tmpDir}


\title{Global options for the raster package}

\description{
Set, inspect, reset, save a number of global options used by the raster package.

Most of these options are used when writing files to disk. They can be ignored by specific functions if the corresponding argument is provided as an argument to these functions.

The default location is returned by \code{rasterTmpDir}. It is the same as that of the R temp directory but you can change it (for the current session) with \code{rasterOptions(tmpdir="path")}. 

To permanently set any of these options, you can add them to \code{<your R installation>/etc/Rprofile.site>}. For example, to change the default directory used to save temporary files, add a line like this: \code{options(rasterTmpDir='c:/temp/')} to that file. All temporary raster files in that folder that are older than 24 hrs are deleted when the raster package is loaded.

Function \code{tmpDir} returns the location of the temporary files
}

\usage{
rasterOptions(format, overwrite, datatype, tmpdir, tmptime, progress, 
     timer, chunksize, maxmemory, todisk, setfileext, tolerance, 
     standardnames, depracatedwarnings, addheader, estimatemem, default=FALSE)	


tmpDir(create=TRUE)	 
}


\arguments{
\item{format}{character. The default file format to use. See \code{\link[raster]{writeFormats}}}
\item{overwrite}{logical. The default value for overwriting existing files. If \code{TRUE}, existing files will be overwritten}
\item{datatype}{character. The default data type to use. See \link[raster]{dataType}}
\item{tmpdir}{character. The default location for writing temporary files; See \code{\link{rasterTmpFile}}}
\item{tmptime}{number > 1. The number of hours after which a temporary file will be deleted. As files are deleted when loading the raster package, this option is only useful if you  save this option so that it is loaded when starting a new session}
\item{progress}{character. Valid values are "text",  "window" and "" (the default in most functions, no progress bar)}
\item{timer}{Logical. If \code{TRUE}, the time it took to complete the function is printed}
\item{chunksize}{integer. Maximum number of cells to read/write in a single chunk while processing (chunk by chunk) disk based Raster* objects}
\item{maxmemory}{integer. Maximum number of cells to read into memory. I.e., if a Raster* object has more than this number of cells, \code{\link[raster]{canProcessInMemory}} will return \code{FALSE} }
\item{todisk}{logical. For debugging only. Default is \code{FALSE} and should normally not be changed. If \code{TRUE}, results are always written to disk, even if no filename is supplied (a temporary filename is used)}
\item{setfileext}{logical. Default is \code{TRUE}. If \code{TRUE}, the file extension will be changed when writing (if known for the file type). E.g. GTiff files will be saved with the .tif extension }
\item{tolerance}{numeric. The tolerance used when comparing the origin and resolution of Raster* objects. Expressed as the fraction of a single cell. This should be a number between 0 and 0.5 }
\item{standardnames}{logical. Default is \code{TRUE}. Should \code{\link{names}} be standardized to be syntactically valid names (using \code{\link{make.names}})}
\item{depracatedwarnings}{logical. If \code{TRUE} (the default) a warning is generated when a depracated (obsolete) function is used}
\item{addheader}{character. If not equal to \code{''} (the default) an additional header file is written when a raster format file (grd/gri) is written. Supported formats are as in \code{\link{hdr}}}
\item{default}{logical. If \code{TRUE}, all options are set to their default values}
\item{create}{logical. If \code{TRUE}, the temporary files directory is created if it does not exist}
\item{estimatemem}{logical. If \code{TRUE}, the amount of RAM available is estimated, and the value of maxmemory is ignored}
}

\value{
list of the current options (invisibly). If no arguments are provided the options are printed.
}

\seealso{ \code{\link[base]{options}}, \code{\link[raster]{rasterTmpFile}} }  

\examples{
\dontrun{
rasterOptions()
rasterOptions(chunksize=2e+07) 
}
}

\keyword{ spatial }

