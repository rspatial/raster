
.maxDatatype <- function(x) {
	x <- sort(x)
	x <- x[substr(x, 1, 3)== substr(x[1], 1, 3)] 
	size <- max(as.integer(substr(x, 4, 4)))
	if (substr(x[1], 1, 3) == 'FLT') {
		return( paste('FLT', size, 'S', sep="") )
	} else {
		# need to do better than this
		return( 'INT4S' )
	}
}