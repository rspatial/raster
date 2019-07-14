\name{rasterTmpFile}

\alias{rasterTmpFile}
\alias{removeTmpFiles}
\alias{showTmpFiles}

\title{Temporary files}

\description{
Functions in the raster package create temporary files if the values of an output Raster* object cannot be stored in memory (RAM). This can happen
when no filename is provided to a function and in functions where you cannot provide a filename (e.g. when using 'raster algebra').

Temporary files are automatically removed at the start of each session. During a session you can use \code{showTmpFiles} to see what is there and \code{removeTmpFiles} to delete all the temporary files.  \code{rasterTmpFile} returns a temporary filename. These can be useful when developing your own functions. These filenames consist of  \code{prefix_date_time_pid_rn} where \code{pid} is the process id returned by \code{\link{Sys.getpid}} and \code{rn} is a 5 digit random number. This should make tempfiles unique if created at different times and also when created in parallel processes (different pid) that use \code{\link{set.seed}} and call rasterTmpFile at the same time. It is possible, however, to create overlapping names (see the examples), which is undesirable and can be avoided by setting the prefix argument.
}

\usage{
rasterTmpFile(prefix='r_tmp_')
showTmpFiles()
removeTmpFiles(h=24)
}

\arguments{
  \item{prefix}{Character. Prefix to the filename (which will be followed by 10 random numbers)}
  \item{h}{Numeric. The minimum age of the files in number of hours (younger files are not deleted)}
}


\value{
\code{rasterTmpFile} returns a valid file name

\code{showTmpFiles} returns the names (.grd only) of the files in the temp directory

\code{removeTmpFiles} returns nothing
}


\seealso{ \code{\link{rasterOptions}}, \code{\link[base]{tempfile}} }  

\examples{
\dontrun{
rasterTmpFile('mytemp_')
showTmpFiles()
removeTmpFiles(h=24)
}}

\keyword{ spatial }


