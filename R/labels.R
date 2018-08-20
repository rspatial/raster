

.polygonLabelPosition <- function(x, cex=1) {
	xy <- coordinates(x)
	# make sure that labels are inside of polygons
	sx <- geometry(x)
	k <- extract(sx, xy)
	k <- which(k[,1] != k[,2])
	if (length(k) > 0) {
		for (i in k) {
			pol <- sx[i, ]
			e <- extent(pol)
			p1 <- xy[i, ,drop=FALSE]
			dx <- 0.25 * (e@xmax - e@xmin)
			dy <- 0.25 * (e@ymax - e@ymin)
			fixed <- FALSE
			for (j in 1:4) {
				if (j < 3) {
					p[1,1] <- p1[1,1] - dx
				} else {
					p[1,1] <- p1[1,1] + dx
				}
				if (j %in% c(2,3)) {
					p[1,2] <- p1[1,2] - dy
				} else {
					p[1,2] <- p1[1,2] + dy				
				}
				z <- extract(pol, rbind(p,p))
				if (!is.na(z[1,2])) {
					xy[i, ] <- p
					break
					fixed <- TRUE
				}
			}
			if (!fixed) print(paste(i, 'not fixed'))
		}
	}
	
	# make sure that labels do not overlap?
	xy
}


