
.makeTiles <- function(x, y, filename="", ...) {
	res <- res(y)
	xy <- xyFromCell(y, 1:ncell(y))
	xy1 <- xy - 0.5 * res
	xy2 <- xy + 0.5 * res
	tiles <- list()
	if (length(filename) > 1) {
		stopifnot(length(filename) == ncell(y))
	} else if (filename != '') {
		ext <- extension(filename)
		extension(filename) <- ''
		filename <- paste0(filename, '_', 1:ncell(y), ext)
	} else if (!canProcessInMemory(x)) {
		filename <- rasterTmpFile()	
		ext <- extension(filename)
		extension(filename) <- ''
		filename <- paste0(filename, '_', 1:ncell(y), ext)
	} else {
		filename <- rep("", ncell(y))
	}
	
	for (i in 1:ncell(y)) {
		e <- extent(xy1[i,1], xy2[i,1], xy1[i,2], xy2[i,2])
		tiles[[i]] <- crop(x, e, filename=filename[i], ...)
	}
	tiles
}

