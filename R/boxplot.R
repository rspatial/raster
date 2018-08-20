# Author: Robert J. Hijmans 
# Date :  November 2010
# Version 1.0
# Licence GPL v3
 

if (!isGeneric("boxplot")) {
	setGeneric("boxplot", function(x, ...)
		standardGeneric("boxplot"))
}

setMethod('boxplot', signature(x='RasterStackBrick'), 
	function(x, maxpixels=100000, ...) {
		nl <- nlayers(x)
		cn <- names(x)
		if ( canProcessInMemory(x)) {
			x <- getValues(x)
		} else {
			warning('taking a sample of ', maxpixels, ' cells')
			x <- sampleRegular(x, maxpixels, useGDAL=TRUE)
		}	
		colnames(x) <- cn
		boxplot(x, ...)
	}
)


setMethod('boxplot', signature(x='RasterLayer'), 
	function(x, y=NULL, maxpixels=100000, ...) {
		if (is.null(y)) {
			cn <- names(x)
			if ( canProcessInMemory(x)) {
				x <- getValues(x)
			} else {
				warning('taking a sample of ', maxpixels, ' cells')
				x = sampleRegular(x, maxpixels, useGDAL=TRUE)
			}	
			x <- matrix(x)
			colnames(x) <- cn
			boxplot(x, ...)
		} else {
			s <- stack(x, y)
			if ( canProcessInMemory(s)) {
				s <- getValues(s)
			} else {
				warning('taking a sample of ', maxpixels, ' cells')
				s <- sampleRegular(s, maxpixels, useGDAL=TRUE)
			}	
			cn <- colnames(s)
			if (is.null(cn)) { #apparently this can happen. 
				cn <- c('layer1', 'layer2')
				colnames(s) <- cn
			}
			f <- as.formula(paste(cn[1], '~', cn[2]))
			boxplot(f, data=s, ...)
		}	
	}
)

