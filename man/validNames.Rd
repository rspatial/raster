\name{validNames}

\alias{validNames}
  
\title{Create valid names}

\description{
Create a set of valid names (trimmed, no duplicates, not starting with a number). 
}

\usage{
validNames(x, prefix='layer')
}

\arguments{
  \item{x}{character}
  \item{prefix}{character string used if x is empty}  
}

\value{
character 
}


\seealso{ \code{\link{make.names} } }


\examples{
validNames(c('a', 'a', '', '1', NA, 'b', 'a'))
}

