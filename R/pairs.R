 
 

setMethod('pairs', signature(x='RasterStackBrick'), 
	function(x, hist=TRUE, cor=TRUE, use="pairwise.complete.obs",  maxpixels=100000, ...) {
	
		panelhist <- function(x,...)	{
			usr <- graphics::par("usr"); on.exit(graphics::par(usr))
			graphics::par(usr = c(usr[1:2], 0, 1.5) )
			h <- hist(x, plot = FALSE)
			breaks <- h$breaks
			nB <- length(breaks)
			y <- h$counts
			y <- y/max(y)
			graphics::rect(breaks[-nB], 0, breaks[-1], y, col="green")
		}
		
		panelcor <- function(x, y,...) {
			usr <- graphics::par("usr")
			on.exit(graphics::par(usr))
			graphics::par(usr = c(0, 1, 0, 1))
			r <- abs(stats::cor(x, y, use=use))
			txt <- format(c(r, 0.123456789), digits=2)[1]
			text(0.5, 0.5, txt, cex = max(0.5, r * 2))
		}
	
		if (hist) {dp <- panelhist} else {dp <- NULL}
		if (cor) {up <- panelcor} else {up <- NULL}
	
	
		d <- sampleRegular(x, maxpixels)
	
		dots <- list(...) 
		cex <- dots$cex
		main <- dots$main
		if (is.null(cex)) cex <- 0.5
		if (is.null(main)) main <- ''
	
		pairs(d, main=main, cex=cex, upper.panel=up, diag.panel=dp)
	}
)

