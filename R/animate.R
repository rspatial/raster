
if (!isGeneric("animate")) {
	setGeneric("animate", function(x, ...)
		standardGeneric("animate"))
}	

setMethod('animate', signature(x='RasterStackBrick'), 
function(x, pause=0.25, main, zlim, maxpixels=50000, n=10, ...) {
	nl <- nlayers(x)
	if (missing(main)) {
		main <- getZ(x)
		if (is.null(main)) {
			main <- names(x)
		}
	}

	x <- sampleRegular(x, size=maxpixels, asRaster=TRUE, useGDAL=TRUE)
	
	if (missing(zlim)) {
		zlim <- c(min(minValue(x)), max(maxValue(x)))
	}
	
	i <- 1
	reps <- 0
    while (reps < n) {
        plot(x[[i]], main = main[i], zlim=zlim, maxpixels=Inf, ...)
        dev.flush()
        Sys.sleep(pause)
        i <- i + 1
        if (i > nl) {
            i <- 1
			reps <- reps+1
		}
    }
}
)

#anim(st, tvals)
