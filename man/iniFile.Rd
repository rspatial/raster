\name{inifile}

\alias{readIniFile}

\title{Read a .ini file}

\description{
This function reads  \code{'.ini'}  files. These are text file databases that are organized in [sections] containing pairs of "name = value".
}

\usage{
readIniFile(filename, token='=', commenttoken=';', aslist=FALSE, case)
}

\arguments{
  \item{filename}{Character. Filename of the .ini file}
  \item{token}{Character. The character that separates the "name" (variable name) from the "value"}
  \item{commenttoken}{Character. This token and everything that follows on the same line is considered a 'comment' that is not for machine consumption and is ignored in processing} 
  \item{aslist}{Logical. Should the values be returned as a list}
  \item{case}{Optional. Function that operates on the text, such as \code{\link{toupper}} or  \code{\link{tolower}} }
}

\details{
This function allows for using inistrings that have "=" as part of a value (but the token cannot be part of the 'name' of a variable!). Sections can be missing. 
}


\value{
A n*3 matrix of characters with columns: section, name, value; or a list if \code{aslist=TRUE}.
}



\keyword{file}
