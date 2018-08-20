

.filler <- function(x, y, maxv=12, circular=FALSE) {

# should rewrite this using apply (or C)

	fill <- function(x, y) {
		r <- matrix(NA, nrow=length(x), ncol=maxv)
		if (circular) {
			for (i in 1:nrow(r)) {
				if (!is.na(y[i])) {
					if (x[i] < y[i]) {
						r[i, x[i]:y[i]] <- 1
					} else {
						r[i, c(x[i]:maxv, 1:y[i])] <- 1	
					}
				}
			}
			r
		} else {
			for (i in 1:nrow(r)) {
				if (!is.na(y[i])) {
					r[i, x[i]:y[i]] <- 1
				}
			}
			r
		}
	}
	x <- overlay(x, y, fun=fill)
	names(x) = paste('v', 1:maxv, sep='')
	x
}


