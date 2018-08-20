

colortable <- function(x) {
	if (.hasSlot(x, 'legend')) {
		x@legend@colortable
	} else {
		logical(0)
	}
}


'colortable<-' <- function(x, value) {
	# for now assuming values are between 0 and 255!!
	x@legend@colortable <- value
	return(x)
}
