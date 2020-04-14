

.simplifyPolygons <- function(p) {
	g <- geom(p)
	out <- NULL
	for (i in 1:g[nrow(g), 'cump']) {
		gg <- g[g[,3]==i, ]
		keep <- rep(TRUE, nrow(gg))
		for (j in 2:(nrow(gg)-1)) {
			if (gg[j,'x'] == gg[j-1,'x'] & gg[j,'x'] == gg[j+1,'x']) {
				keep[j] <- FALSE
			} else if (gg[j,'y'] == gg[j-1,'y'] & gg[j,'y'] == gg[j+1,'y']) {
				keep[j] <- FALSE				
			}
		}
		gg <- gg[keep, ]
		out <- rbind(out, gg)
	}
	out <- as(data.frame(out), 'SpatialPolygons')
	crs(out) <- proj4string(p)
	if (.hasSlot(p, 'data')) {
		out <- SpatialPolygonsDataFrame(out, p@data)
	}
	out
}

