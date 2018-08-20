
.cor <- function(x, n=Inf, ...) {
		
		nl <- nlayers(x)
		if (nl < 2) return(1)
		
		if (n < ncell(x)) {
			x <- sampleRegular(x, size=n, asRaster=TRUE)
		}
		
		if (canProcessInMemory(x, nlayers(x)*4)) {
			s <- stats::na.omit(getValues(x))
			s <- stats::cor(s)
		} else {
			msk <- sum(x, na.rm=FALSE)
			x <- mask(x, msk)
			mx <- cellStats(x, 'mean')
			sx <- cellStats(x, 'sd')
			nc <- ncell(x)
			s <- matrix(NA, nrow=n, ncol=n)
			for (i in 1:(nl-1)) {
				for (j in (i+1):nl) {
					s[j,i] <- s[i,j] <- cellStats(((x[[i]] - mx[i]) * (x[[j]] - mx[j])) / (sx[i] * sx[j]), sum)/ (nc-1)
				}
			}
			diag(s) <- 1			
		}
		if (nrow(s) == 2) {
			s[2,1]
		} else {
			colnames(s) <- rownames(s) <- names(x)
			s		
		}
}




