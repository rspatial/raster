
# if (!isGeneric(".quad")) {
	# setGeneric(".quad", function(x, ...)
		# standardGeneric(".quad"))
# }	



# setMethod('.quad', signature(x='missing'), 
	# function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, levels=1, steps=1, crs) {
		# e <- extent(xmn, xmx, ymn, ymx)
		# if (missing(crs)) {
			# if (e@xmin > -400 & e@xmax < 400 & e@ymin > -90.1 & e@ymax < 90.1) { 
				# crs <- "+proj=longlat +datum=WGS84"
			# } else {
				# crs <- ""
			# }
		# }
		# b <- .quad(e, nrows=nrows, ncols=ncols, crs=crs, levels=levels, steps=steps)
		# return(b)
	# }
# )


# setMethod('.quad', signature(x='Extent'), 
	# function(x, nrows=10, ncols=10, levels=1, steps=1, crs='') {
		# bb <- extent(x)
		# nr = as.integer(round(nrows))
		# nc = as.integer(round(ncols))
		# if (nc < 1) { stop("ncols should be > 0") }
		# if (nr < 1) { stop("nrows should be > 0") }
		# b <- methods::new("RasterQuadBrick", extent=bb, ncols=nc, nrows=nr)
		# projection(b) <- crs
		# levels <- as.integer(max(round(levels), 0))
		# steps <- as.integer(max(round(steps), 0))
		# nl <- levels * steps
		# b@nlevels <- levels
		# b@nsteps <- steps
		# b@data@nlayers <- as.integer(nl)
		# return(b) 
	# }
# )

