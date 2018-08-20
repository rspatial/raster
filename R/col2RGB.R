
.col2RGB <- function(x) {
	d <- t( col2rgb(x@legend@colortable) )
	d <- data.frame(id=0:255, d)
	subs(x, d, which=2:4)
}


.alphaCT <- function(x, alpha) {
	ct <- colortable(x)
	z <- t(col2rgb(ct))
	ct <- apply(z, 1, function(i) grDevices::rgb(i[1], i[2], i[3], alpha*255, maxColorValue=255))
	colortable(x) <- ct
	return(x)
}
