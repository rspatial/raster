\name{match}

\docType{methods}

\alias{match}
\alias{match,Raster-method}
\alias{\%in\%}
\alias{\%in\%,Raster-method}

\title{Value matching for Raster* objects}

\description{
\code{match} returns a Raster* object with the position of the matched values. The cell values are the index of the table argument. 

\code{\%in\%} returns a logical Raster* object indicating if the cells values were matched or not.

}

\usage{
match(x, table, nomatch = NA_integer_, incomparables = NULL)

x \%in\% table
}

\arguments{
 \item{x}{Raster* object}
 \item{table}{vector of the values to be matched against}
 \item{nomatch}{the value to be returned in the case when no match is found. Note that it is coerced to integer}
 \item{incomparables}{a vector of values that cannot be matched. Any value in x matching a value in this vector is assigned the nomatch value. For historical reasons, FALSE is equivalent to NULL}
}

\value{
Raster* obeject
}

\seealso{ \code{\link{calc}, \link[base]{match}} }




\examples{
r <- raster(nrow=10, ncol=10)
r[] <- 1:100
m <- match(r, c(5:10, 50:55))
n <- r \%in\% c(5:10, 50:55)
}

\keyword{spatial}
\keyword{methods}
