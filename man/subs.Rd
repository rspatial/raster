\name{substitute}

\docType{methods}

\alias{subs}
\alias{subs,Raster,data.frame-method}

\title{ Substitute values in a Raster* object}

\description{
Substitute (replace) values in a Raster* object with values in a \code{data.frame}. The \code{data.frame} should have a column to identify the key (ID) to match with the cell values of the Raster* object, and one or more columns with replacement values. By default these are the first and second column but you can specify other columns with arguments \code{by} and \code{which}. It is possible to match one table to multiple layers, or to use multiple layers as a single key, but not both.
}

\usage{
\S4method{subs}{Raster,data.frame}(x, y, by=1, which=2, subsWithNA=TRUE, filename='', ...)
}


\arguments{
  \item{x}{Raster* object}
  \item{y}{data.frame}
  \item{by}{column number(s) or name(s) identifying the key (ID) to match rows in data.frame \code{y} to values of the Raster object}
  \item{which}{column number or name that has the new (replacement) values}
  \item{subsWithNA}{logical. If \code{TRUE} values that are not matched become NA. If \code{FALSE}, they retain their original value (which could also be \code{NA}). This latter option is handy when you want to replace only one or a few values. It cannot be used when \code{x} has multiple layers}
  \item{filename}{character. Optional output filename}
  \item{...}{additional arguments as for \code{\link{writeRaster}}}  
}


\details{
You could obtain the same result with \code{\link[raster]{reclassify}}, but \code{subs} is more efficient for simple replacement. Use \code{reclassify} if you want to replace ranges of values with new values. 

You can also replace values using a fitted model. E.g. fit a model to \code{glm} or \code{loess} and then call \link[raster]{predict}
}

\value{
Raster object
}



\seealso{ \code{\link{reclassify}, \link{clamp}, \link{cut}}}

\examples{
r <- raster(ncol=10, nrow=10)
r[] <- round(runif(ncell(r)) * 10)
df <- data.frame(id=2:8, v=c(10,10,11,11,12:14))
x <- subs(r, df)
x2 <- subs(r, df, subsWithNA=FALSE)

df$v2 <- df$v * 10
x3 <- subs(r, df, which=2:3)

s <- stack(r, r*3)
names(s) <- c('first', 'second')
x4 <- subs(s, df)
x5 <- subs(s, df, which=2:3)
}

\keyword{methods}
\keyword{spatial}

