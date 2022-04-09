# Author: Robert J. Hijmans
# Date: Sept 2008
# Version 0.9
# Licence GPL v3


pbCreate <- function(nsteps, progress, style=3, label='Progress', ...) {

	if (missing(progress)) {
		progress <- .progress()
	}

	if (is.null(progress)) {
		progress <- .progress()
	}
	
	if (progress=='text') {
		pb <- utils::txtProgressBar(min=0, max=nsteps, style=style)
	} else if (progress %in% c('window', 'tcltk', 'windows')) {
		tit <- paste(label, ' (', nsteps, ' steps)', sep='')
		#if (.Platform$OS.type == "windows" ) {
		#	pb <- winProgressBar(title=tit, min=0 , max=nsteps, width = 300, label='starting')
		#} else {
		requireNamespace("tcltk")
		pb <- tcltk::tkProgressBar(title=tit, min=0, max=nsteps, width = 300, label='starting')
		#}
	} else {
		pb <- 'none'
	}
	attr(pb, "starttime") <- Sys.time()
	return(pb)
}



pbStep <- function(pb, step=NULL, label='') {
	pbclass <- class(pb)
	if (inherits(pbclass, "txtProgressBar")) {
		if (is.null(step)) { step = pb$getVal() + 1 }
		utils::setTxtProgressBar(pb, step)
	} else if (inherits(pbclass,"tkProgressBar")) {
		if (is.null(step)) { step = pb$getVal() + 1 }
		tcltk::setTkProgressBar(pb, step, label=paste(label, step))	
	#} else if (pbclass=="winProgressBar") {
	#	if (is.null(step)) { step <- getWinProgressBar(pb)+1  }
	#	setWinProgressBar(pb, step, label=paste(label, step))	
	} 
}

pbClose <- function(pb, timer) {
	pbclass <- class(pb)
	if (inherits(pbclass, "txtProgressBar")) {
		cat("\n\r")
		close(pb)
	} else if (inherits(pbclass, "tkProgressBar")) {
		close(pb)
	}
	if (missing(timer)) {
		timer <- .timer()		
	}
	if (timer) {
		elapsed <- difftime(Sys.time(), attr(pb, "starttime"), units = "secs")
		cat(round(as.numeric(elapsed)), 'seconds\n')
	}
}


