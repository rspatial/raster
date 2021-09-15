

setAs("data.frame", "SpatialPolygons",
	function(from) {
		v <- colnames(from)[5]
		
		if (v == "x") {
			obs <- unique(from$object)
			sp <- list()
			for (i in 1:length(obs)) {
				s <- from[from$object==obs[i], ]
				p <- unique(s$part)
				pp <- list()
				for (j in 1:length(p)) {
					ss <- s[s$part==p[j], ]
					pol <- sp::Polygon( as.matrix(ss[,c('x', 'y')] ))
					if (ss$hole[1]) {
						pol@hole <- TRUE
					}
					pp[[j]] <- pol
				}
				sp[[i]] <- sp::Polygons(pp, as.character(i))
			}
			
		} else if (v == "hole") {
			colnames(from)[1] <- "id"
			obs <- unique(from$id)
			sp <- list()
			for (i in 1:length(obs)) {
				s <- from[from$id==obs[i], ]
				p <- unique(s$part)
				pp <- list()
				jj <- 1
				for (j in 1:length(p)) {
					ss <- s[s$part==p[j], ]
					hi <- ss$hole > 0
					holes <- ss[hi, ]
					ss <- ss[!hi,]
					pol <- sp::Polygon( as.matrix(ss[,c("x", "y")] ))
					pp[[jj]] <- pol
					jj <- jj + 1
					if (nrow(holes) > 0) {
						uh <- unique(holes$hole)
						for (k in uh) {
							pol <- sp::Polygon( as.matrix(holes[holes$hole==k, c("x", "y")] ))
							pol@hole <- TRUE
							pp[[jj]] <- pol	
							jj <- jj + 1
						}
					}
					sp[[i]] <- sp::Polygons(pp, as.character(i))
				}		
			}
		} else {
			stop("cannot process this data.frame")
		}
		sp::SpatialPolygons(sp)
	}
)


setAs("data.frame", "SpatialPolygonsDataFrame",
	function(from) {
		x <- as(from, "SpatialPolygons")
		if (ncol(from) > 6) {
			d <- unique(from[, -c(2:6), drop=FALSE])
			rownames(d) <- d$object
			d <- d[, -1, drop=FALSE]
			sp::SpatialPolygonsDataFrame(x, d)
		} else {
			x
		}
	}
)




setAs("data.frame", "SpatialLines",
	function(from) {
		if (colnames(from)[1] == "id") colnames(from)[1] <- "object"
		obs <- unique(from$object)
		sp <- list()
		for (i in 1:length(obs)) {
			s <- from[from$object==obs[i], ]
			p <- unique(s$part)
			pp <- list()
			for (j in 1:length(p)) {
				ss <- s[s$part==p[j], ]
				ln <- sp::Line(as.matrix(ss[,c("x", "y")]))
				pp[[j]] <- ln
			}
			sp[[i]] <- sp::Lines(pp, as.character(i))
		}
		sp::SpatialLines(sp)
	}
)


setAs("data.frame", "SpatialLinesDataFrame",
	function(from) {
		x <- as(from, "SpatialLines")
		if (ncol(from) > 5) {
			d <- unique(from[, -c(2:5), drop=FALSE])
			rownames(d) <- d$object
			d <- d[, -1, drop=FALSE]
			sp::SpatialLinesDataFrame(x, d)
		} else {
			x
		}
	}
)


