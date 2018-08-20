

setAs('data.frame', 'SpatialPolygons',
	function(from) {
		obs <- unique(from$object)
		sp <- list()
		for (i in 1:length(obs)) {
			s <- from[from$object==obs[i], ]
			p <- unique(s$part)
			pp <- list()
			for (j in 1:length(p)) {
				ss <- s[s$part==p[j], ]
				pol <- Polygon( as.matrix(ss[,c('x', 'y')] ))
				if (ss$hole[1]) {
					pol@hole <- TRUE
				}
				pp[[j]] <- pol
			}
			sp[[i]] <- Polygons(pp, as.character(i))
		}
		
		SpatialPolygons(sp)
	}
)


setAs('data.frame', 'SpatialPolygonsDataFrame',
	function(from) {
		x <- as(from, 'SpatialPolygons')
		if (ncol(from) > 6) {
			d <- unique(from[, -c(2:6), drop=FALSE])
			rownames(d) <- d$object
			d <- d[, -1, drop=FALSE]
			SpatialPolygonsDataFrame(x, d)
		} else {
			x
		}
	}
)




setAs('data.frame', 'SpatialLines',
	function(from) {
		obs <- unique(from$object)
		sp <- list()
		for (i in 1:length(obs)) {
			s <- from[from$object==obs[i], ]
			p <- unique(s$part)
			pp <- list()
			for (j in 1:length(p)) {
				ss <- s[s$part==p[j], ]
				ln <- Line(as.matrix(ss[,c('x', 'y')]))
				pp[[j]] <- ln
			}
			sp[[i]] <- Lines(pp, as.character(i))
		}
		SpatialLines(sp)
	}
)


setAs('data.frame', 'SpatialLinesDataFrame',
	function(from) {
		x <- as(from, 'SpatialLines')
		if (ncol(from) > 5) {
			d <- unique(from[, -c(2:5), drop=FALSE])
			rownames(d) <- d$object
			d <- d[, -1, drop=FALSE]
			SpatialLinesDataFrame(x, d)
		} else {
			x
		}
	}
)


