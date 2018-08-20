# Oscar Perpinan Lamigueiro

zApply <- function(x, by, fun=mean, name='', ...){
	z <- getZ(x)
	stopifnot(length(z) == nlayers(x))

	##from aggregate.zoo
	my.unique <- function(x) x[match(x, x) == seq_len(length(x))] 
	my.sort <- function(x) x[order(x)]
	
    if (is.function(by)) { by <- by(z) }
    ##stopifnot(length(time(x)) == length(by))
    b <- stackApply(x, as.numeric(factor(by)), match.fun(fun), ...)
    zval <- my.sort(my.unique(by))
    b <- setZ(b, zval, name)
    names(b) <- as.character(zval)
    b
}


