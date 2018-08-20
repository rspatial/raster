\name{SampleInt}

\alias{sampleInt}

\title{Sample integer values}

\description{
Take a random sample from a range of integer values between 1 and \code{n}. Its purpose is similar to that of \code{\link[base]{sample}}, but that function fails when \code{n} is very large.
}

\usage{
sampleInt(n, size, replace=FALSE)
}

\arguments{
  \item{n}{Positive number (integer); the number of items to choose from }
  \item{size}{Non-negative integer; the number of items to choose}  
  \item{replace}{Logical. Should sampling be with replacement?}
 }


\value{vector of integer numbers}



\examples{
  sampleInt(1e+12, 10)
  
# this may fail:
#  sample.int(1e+12, 10)
#  sample.int(1e+9, 10)
}


\keyword{spatial}
