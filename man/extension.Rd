\name{extension}

\alias{extension}
\alias{extension<-}

\title{Filename extensions}

\description{
Get or change a filename extension
}

\usage{
extension(filename, value=NULL, maxchar=10)
extension(filename) <- value
}

\arguments{
\item{filename}{A filename, with or without the path}
\item{value}{A file extension with or without a dot, e.g., ".txt" or "txt"}
\item{maxchar}{Maximum number of characters after the last dot in the filename, for that string to be considered a filename extension }
}

\value{
A file extension, filename or path.  

If \code{ext(filename)} is used without a \code{value} argument, it returns the file extension; otherwise it returns the filename (with new extensions set to \code{value}
}




\examples{
fn <- "c:/temp folder/filename.exten sion"
extension(fn)
extension(fn) <- ".txt"
extension(fn)
fn <- extension(fn, '.document')
extension(fn)
extension(fn, maxchar=4)
}

\keyword{file}

