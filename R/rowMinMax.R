
.rowMin <- function(x, na.rm=TRUE) {
#  .Call('raster_doRowMin', PACKAGE = 'raster', x, narm=na.rm)
  .doRowMin(x, narm=na.rm)
}

.rowMax <- function(x, na.rm=TRUE) {
  .doRowMax(x, narm=na.rm)
}

.colMin <- function(x, na.rm=TRUE) {
  .doRowMin(t(x), narm=na.rm)
}

.colMax <- function(x, na.rm=TRUE) {
  .doRowMax(t(x), narm=na.rm)
}


