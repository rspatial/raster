
### this is the kde2d function from the MASS packlage with minimal changes
.kde2d <- function (x, y, h, n, lims) {
    nx <- length(x)
    gx <- seq.int(lims[1L], lims[2L], length.out = n[1L])
    gy <- seq.int(lims[3L], lims[4L], length.out = n[2L])
    h <- h/4
    ax <- outer(gx, x, "-")/h[1L]
    ay <- outer(gy, y, "-")/h[2L]
    tcrossprod(matrix(stats::dnorm(ax), , nx), matrix(stats::dnorm(ay), , nx))/(nx * h[1L] * h[2L])
}



.kernelDens <- function(p, x, bandwidth, ...) {
	
	.bandwidth.nrd <- function(x) {
	### this function is from the MASS package
		r <- stats::quantile(x, c(0.25, 0.75))
		h <- (r[2L] - r[1L])/1.34
		4 * 1.06 * min(sqrt(stats::var(x)), h) * length(x)^(-1/5)
	}
	
    if(missing(bandwidth)) {
		bw <- c(.bandwidth.nrd(p[,1]), .bandwidth.nrd(p[,2]))
	} else {
		bw <- rep(bandwidth, length.out = 2L)
	}
	v <- .kde2d(p[,1], p[,2], bw, dim(x)[1:2], as.vector(t(bbox(x))))
	v <- t(v)
	v <- v[nrow(v):1, ]
	setValues(x, v)
}

#a = kernelDens(xy, r)

