

.randomize <- function(x, ...) {
	if (!hasValues(x)) {
		return(x)
	}
	nl <- nlayers(x)
	if (nl > 1) {
		y <- brick(x, values=FALSE)
		for (i in 1:nl) {
			y <- setValues(y, sample(getValues(x[[i]])), layer=i)
		}
		y
	} else {
		setValues(x, sample(getValues(x)))
	}
}

