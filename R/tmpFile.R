# Author: Robert J. Hijmans
# Date :  May 2009
# Version 0.9
# Licence GPL v3


.fileSaveDialog <- function(filetypes="") {
	if (! requireNamespace("tcltk") ) {
		stop('you need to install the tcltk library')
	}
	if (filetypes == "") {
		filetypes="{{GeoTIFF} {.tif} } {{grid files} {.grd}}"
	}
	tcltk::tclvalue(tcltk::tkgetSaveFile(filetypes=filetypes))
}

.fileOpenDialog <- function(filetypes="") {
	if (! requireNamespace("tcltk") ) {
		stop('you need to install the tcltk library')
	}
	if (filetypes == "") {
		filetypes="{{All Files} *} {{GeoTIFF} {.tif} } {{grid files} {.grd}}"
	}
	tcltk::tclvalue(tcltk::tkgetOpenFile(filetypes=filetypes))
}


.old_rasterTmpFile <- function(prefix='raster_tmp_')  {
	f <- getOption('rasterTmpFile')
	if (!is.null(f)) {
		f <- trim(f)
		if (! f == '' ) {
			options('rasterTmpFile' = NULL)
			return(f)
		}
	}
	
	extension <- .defaultExtension(.filetype())
	d <- tmpDir(create=TRUE)
#	dir.create(d,  showWarnings = FALSE)
	f <- paste(round(stats::runif(10)*10), collapse="")
	d <- paste(d, prefix, f, extension, sep="")
	if (file.exists(d)) {
		d <- rasterTmpFile(prefix=prefix)
	}
	if (getOption('verbose')) { cat('writing raster to:', d) }
	return(d)
}




rasterTmpFile <- function(prefix='r_tmp_')  {
	f <- getOption('rasterTmpFile')
	if (!is.null(f)) {
		f <- trim(f)
		if (! f == '' ) {
			options('rasterTmpFile' = NULL)
			return(f)
		}
	}

	extension <- .defaultExtension(.filetype())
	d <- tmpDir()

	while(TRUE) {
	# added pid as suggested by Daniel Schlaepfer to avoid overlapping file names when running parallel processes and using set.seed() in each node
		f <- paste(prefix, gsub(" ", "_", gsub(":", "", as.character(Sys.time()))), "_", Sys.getpid(), "_", paste(sample(0:9,5,replace=TRUE),collapse=''), extension, sep = "")
		tmpf <- normalizePath(file.path(d, f), winslash = "/", mustWork=FALSE)
		if (! file.exists(tmpf)) {
			break
		}
	}
	
	if (getOption('verbose')) { 
		cat('writing raster to:', tmpf) 
	}
	return(tmpf)
}


.removeTrailingSlash <- function(d) {
		if (substr(d, nchar(d), nchar(d)) == '/') { d <- substr(d, 1, nchar(d)-1) }
		if (substr(d, nchar(d), nchar(d)) == '\\') { d <- substr(d, 1, nchar(d)-1) }
		return(d)
}


removeTmpFiles <- function(h=24) {
	
# remove files in the temp folder that are > h hours old	
	warnopt <- getOption('warn')
	on.exit(options('warn'= warnopt))

	tmpdir <- tmpDir(create=FALSE)
	if (!is.na(tmpdir)) {
	
		d <- .removeTrailingSlash(tmpdir)
		f <- list.files(path=d, pattern='r_tmp*', full.names=TRUE, include.dirs=TRUE)
#		f <- list.files(path=d, pattern='[.]gr[di]', full.names=TRUE, include.dirs=TRUE)
		fin <- file.info(f)
		dif <- Sys.time() - fin$mtime
		dif <- as.numeric(dif, units="hours")
		
		f <- f[which(dif > h)]
		unlink(f, recursive=TRUE)
	}	
	options('warn'=warnopt) 
}



showTmpFiles <- function() {
	f <- NULL
	tmpdir <- tmpDir(create=FALSE)
	if (!is.na(tmpdir)) {
		d <- .removeTrailingSlash(tmpdir)
		if (file.exists(d)) {
			f <- list.files(d, pattern='r_tmp_')
			#f <- list.files(d, pattern='\\.gri$')
			if (length(f) == 0) {
				cat('--- none ---\n')
			} else {
				ff <- f
				extension(ff) <- ''
				ff <- paste(unique(ff), '\n', sep='')
				cat(ff)
			}
		} else {
			cat('--- none ---\n')
		}
	} else {
		cat('--- none ---\n')
	}
	invisible(f)
}

