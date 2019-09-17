

setMethod("as.character", signature(x="Extent"), 
	function(x, ...) {
		e <- extent(x)
		paste0("extent(", paste(as.vector(e), collapse=", "), ")")
	}
)


setMethod("as.character", signature(x="Raster"), 
	function(x, ...) {
		e <- extent(x)
		crs <- crs(x)
		crs <- ifelse(is.na(crs), ", crs=''", paste0(", crs='", crs, "'"))
		if (nlayers(x) < 2) {
			paste0("raster(", 
				"ncol=",ncol(x),
				", nrow=",nrow(x),
				", xmn=",e[1],
				", xmx=",e[2],
				", ymn=",e[3],
				", ymx=",e[4],
				crs, ")" 
			)
		
		} else {
			paste0("brick(", 
				"ncol=", ncol(x),
				", nrow=", nrow(x),
				", nl=", nlayers(x),
				", xmn=",e[1],
				", xmx=",e[2],
				", ymn=",e[3],
				", ymx=",e[4],
				crs, ")" 
			)
		}
	}
)
#eval(parse(text=as.character(raster())))
#eval(parse(text=as.character(stack())))
