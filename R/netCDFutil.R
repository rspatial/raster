# Author: Robert J. Hijmans
# Date: June 2010
# Version 1.0
# Licence GPL v3


.getCRSfromGridMap4 <- function(g) {

	sp <- g$standard_parallel
	if (length(sp) > 1) {
		g$standard_parallel1 <- sp[1]
		g$standard_parallel2 <- sp[2]
		g$standard_parallel <- NULL
	}

	vals <- sapply(g, function(i) i[1]) 
	vars <- names(vals)
	if (any(vars == "epsg_code")) {
		crs <- vals[vars=="epsg_code"] 
		crs <- paste0("+init=epsg:", crs)
		return(crs)
	} else if (any(vars %in% c("proj4", "crs_wkt", "spatial_ref"))) {
		crs=vals[vars %in% c("proj4", "crs_wkt", "spatial_ref")][1]
		return(crs)
	}
# based on info at 
# http://trac.osgeo.org/gdal/wiki/NetCDF_ProjectionTestingStatus
# accessed 7 October 2012
	prj <- matrix(c("albers_conical_equal_area", "aea", "azimuthal_equidistant", "aeqd", "lambert_cylindrical_equal_area", "cea", "lambert_azimuthal_equal_area", "laea", "lambert_conformal_conic", "lcc", "latitude_longitude", "longlat", "mercator", "merc", "orthographic", "ortho", "polar_stereographic", "stere", "stereographic", "stere", "transverse_mercator", "tmerc"), ncol=2, byrow=TRUE)
	
	m <- matrix(c("grid_mapping_name", "+proj", "false_easting", "+x_0","false_northing", "+y_0", "scale_factor_at_projection_origin", "+k_0", "scale_factor_at_central_meridian", "+k_0", "standard_parallel", "+lat_1", "standard_parallel1", "+lat_1", "standard_parallel2", "+lat_2", "longitude_of_central_meridian", "+lon_0", "longitude_of_projection_origin", "+lon_0", "latitude_of_projection_origin", "+lat_0", "straight_vertical_longitude_from_pole", "+lon_0",
	"longitude_of_prime_meridian", "+pm", "semi_major_axis", "+a", "semi_minor_axis", "+b", "inverse_flattening", "+rf", 
	"earth_radius", "+a"), ncol=2, byrow=TRUE)

	# add logic that if prime merid is defined but not centr merid. centr merid is same as prime.
		
	i <- match(vars, m[,1])
	if (all(is.na(i))) {
		gg <- cbind(vars, vals)
		mtxt <- paste(apply(gg, 1, function(x) paste(x, collapse='=')), collapse='; ')
		warning("cannot process the crs\n", mtxt)
		return(NA)
	} else if (any(is.na(i))) {
		vr <- vars[is.na(i)]
		vl <- vals[is.na(i)]
		gg <- cbind(vr, vl)
		gg <- gg[!(gg[,1] %in% c("crs_wkt", "esri_pe_string")), ,drop=FALSE]
		if (NROW(gg) > 0) {
			mtxt <- paste(apply(gg, 1, function(x) paste(x, collapse='=')), collapse='\n')
			warning("cannot process these parts of the crs:\n", mtxt)
		}	
		vars <- vars[!is.na(i)]
		vals <- vals[!is.na(i)]
		i <- stats::na.omit(i)
	}
	tab <- cbind(m[i,], vals)
	rr <- which(tab[,1] == "earth_radius")
	if (length(rr) > 0) {
		bb <- tab[rr,]
		bb[2] <- "+b"
		tab <- rbind(tab, bb)
	}
	p <- which(tab[,2] == '+proj')
	if (length(p) == 0) {
		warning("cannot create a valid crs\n", mtxt)
		return(NA)	
	} else {
		tab <- rbind(tab[p, ], tab[-p, ])
	}
	j <- match(tab[1,3], prj[,1])
	tab[1,3] <- prj[j,2]
	cr <- paste(apply(tab[,2:3], 1, function(x) paste(x, collapse='=')), collapse=' ')
	crtst <- try(sp::CRS(cr), silent=TRUE)
	if ( inherits(crtst, "try-error")) {
		mtxt <- paste(m, collapse='; ')
		warning("cannot create a valid crs\n", mtxt)
		return(NA)
	} else {
		return(cr)
	}
}


.isNetCDF <- function(x) {
	on.exit(options('warn'= getOption('warn')))
	options('warn'=-1) 
	fcon <- file(x, "rb")
	tst <- try( w <- readBin(fcon, what='character', n=1), silent=TRUE)
	close(fcon)
	if ( isTRUE((substr(w, 1, 3) == "CDF" ))) { return(TRUE) 
	} else { return(FALSE)
	}
}


.getRasterDTypeFromCDF <- function(type) { 
	if (type == "char" )  { return("INT1U") 
	} else if (type == "byte" ) { return("INT1S")
	} else if (type == "short" ) { return("INT2S")
	} else if (type == "int" ) { return("INT4S")
	} else if (type == "integer" ) { return("INT4S")
	} else if (type == "float" ) { return("FLT4S")
	} else if (type =="double" ) { return("FLT8S") 
	} else { return("FLT4S") }
}


.getNetCDFDType <- function(dtype) {
	if (!(dtype %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT1U', 'INT2U', 'FLT4S', 'FLT8S'))) {
		stop('not a valid data type')
	}
	type <- .shortDataType(dtype)
	size <- dataSize(dtype) * 8
	signed <- dataSigned(dtype)
	
	if (size == 8) {
		if (!signed) {
			return("char") #8-bit characters intended for representing text.
		} else {
			return("byte")
		}
	} else if (type == 'INT') {
		if (!signed) {
			warning('netcdf only stores signed integers')
		}
		if (size == 16) { 
			return( "short" ) 
		} else if (size == 32 ) { 
			return( "integer" ) 
		} else {
			return ( "double" )		
		}
	} else {
		if (size == 32) { 
			return( "float" ) 
		} else {  
			return ( "double" )  
		}
	}
}


