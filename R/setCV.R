
.setCV <- function(x, v, col) {
	stopifnot(length(v) == (length(col)+1))
	v <- as.numeric(v)
	x@legend@values <- v
	x@legend@color <- col
	x@legend@colortable <- vector()
	x	
}

#val <- c(-1, -0.3, -0.2, 0, 0.1, 0.3, 0.4, 0.6, 0.8, 1, 10)
#ct <- c(grDevices::col2rgb("white"),grDevices::col2rgb("blue"),grDevices::rgb(205,193,173, maxColorValue = 255), grDevices::rgb(150,150,150, maxColorValue = 255), grDevices::rgb(120,100,51, maxColorValue = 255), grDevices::rgb(120,200,100, maxColorValue = 255), grDevices::rgb(28,144,3, maxColorValue = 255), grDevices::rgb(6,55,0, maxColorValue = 255), grDevices::rgb(10,30,25, maxColorValue = 255), grDevices::rgb(6,27,7, maxColorValue = 255))

.setCT <- function(x, v, col, na='white') {
	v <- as.numeric(v)
	na <- which(is.na(v))
	if (length(na)==0) {
		v <- c(NA, v)
		col <- c('white', col)
	} else {
		v <- c(v[na], v[-na])
		col <- c(col[na], col[-na])
	}
	notrgb <- which(substr(col, 1, 1) != '#')
	col[notrgb] <-  grDevices::rgb(t(grDevices::col2rgb(col[notrgb])), maxColorValue=255)
	x@legend@values <- v
	x@legend@color <- col
	x	
}

