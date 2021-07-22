# Download geographic data and return as R object
# Author: Robert J. Hijmans
# License GPL3
# Version 0.9
# October 2008


getData <- function(name='GADM', download=TRUE, path='', ...) {
	path <- .getDataPath(path)
	if (name=='GADM') {
		.GADM(..., download=download, path=path)
	} else if (name=='SRTM') {
		.SRTM(..., download=download, path=path)
	} else if (name=='alt') {
		.raster(..., name=name, download=download, path=path)
	} else if (name=='worldclim') {
		.worldclim(..., download=download, path=path)
	} else if (name=='CMIP5') {
		.cmip5(..., download=download, path=path)
	} else if (name=='CMIP6') {
	  .cmip6(..., ddownload=download, path=path)
	} else if (name=='ISO3') {
		ccodes()[,c(2,1)]
	} else if (name=='countries') {
		.countries(download=download, path=path, ...)
	} else {
		stop(name, ' not recognized as a valid name.')
	}
}


.download <- function(aurl, filename) {
	fn <- paste(tempfile(), '.download', sep='')
	res <- utils::download.file(url=aurl, destfile=fn, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
	if (res == 0) {
		w <- getOption('warn')
		on.exit(options('warn' = w))
		options('warn'=-1) 
		if (! file.rename(fn, filename) ) { 
			# rename failed, perhaps because fn and filename refer to different devices
			file.copy(fn, filename)
			file.remove(fn)
		}
	} else {
		stop('could not download the file' )
	}
}

.ISO <- function() {
   ccodes()
}

ccodes <- function() {
	path <- system.file(package="raster")
	#d <- utils::read.csv(paste(path, "/external/countries.csv", sep=""), stringsAsFactors=FALSE, encoding="UTF-8")
	readRDS(file.path(path, "external/countries.rds"))
}


.getCountry <- function(country='') {
	country <- toupper(trim(country[1]))

	cs <- ccodes()
	cs <- sapply(cs, toupper)
	cs <- data.frame(cs, stringsAsFactors=FALSE)
	nc <- nchar(country)

	if (nc == 3) {
		if (country %in% cs$ISO3) {
			return(country)
		} else {
			stop('unknown country')
		}
	} else if (nc == 2) {
		if (country %in% cs$ISO2) {
			i <- which(country==cs$ISO2)
			return( cs$ISO3[i] )
		} else {
			stop('unknown country')
		}
	} else if (country %in% cs[,1]) {
		i <- which(country==cs[,1])
		return( cs$ISO3[i] )
	} else if (country %in% cs[,4]) {
		i <- which(country==cs[,4])
		return( cs$ISO3[i] )
	} else if (country %in% cs[,5]) {
		i <- which(country==cs[,5])
		return( cs$ISO3[i] )
	} else {
		stop('provide a valid name name or 3 letter ISO country code; you can get a list with "ccodes()"')
	}
}


.getDataPath <- function(path) {
	path <- trim(path)
	if (path=="") {
		path <- .dataloc()
	} else {
		if (substr(path, nchar(path)-1, nchar(path)) == '//' ) {
			p <- substr(path, 1, nchar(path)-2)		
		} else if (substr(path, nchar(path), nchar(path)) == '/'  | substr(path, nchar(path), nchar(path)) == '\\') {
			p <- substr(path, 1, nchar(path)-1)
		} else {
			p <- path
		}
		if (!file.exists(p) & !file.exists(path)) {
			stop('path does not exist: ', path)
		}
	}
	if (substr(path, nchar(path), nchar(path)) != '/' & substr(path, nchar(path), nchar(path)) != '\\') {
		path <- paste(path, "/", sep="")
	}
	return(path)
}


.GADM <- function(country, level, download, path, version=3.6, type='sp') {
#	if (!file.exists(path)) {  dir.create(path, recursive=T)  }

	country <- .getCountry(country)
	if (missing(level)) {
		stop('provide a "level=" argument; levels can be 0, 1, or 2 for most countries, and higher for some')
	}

	if (version > 3) {
		if (type == 'sf') {
			filename <- file.path(path, paste0('gadm36_', country, '_', level, "_sf.rds"))
		} else {
			filename <- file.path(path, paste0('gadm36_', country, '_', level, "_sp.rds"))
		}
	} else {
		filename <- paste(path, 'GADM_', version, '_', country, '_', level, ".rds", sep="")	
	}
	
	if (!file.exists(filename)) {
		if (download) {
			baseurl <- paste0("https://biogeo.ucdavis.edu/data/gadm", version)
			if (version == 2.8) {
				theurl <- paste(baseurl, '/rds/', country, '_adm', level, ".rds", sep="")			
			} else {
				if (type == 'sf') {
					theurl <- paste(baseurl, '/Rsf/gadm36_', country, '_', level, "_sf.rds", sep="")			
				} else {
					theurl <- paste(baseurl, '/Rsp/gadm36_', country, '_', level, "_sp.rds", sep="")			
				}
			}
			.download(theurl, filename)
			if (!file.exists(filename))	{ 
				message("\nCould not download file -- perhaps it does not exist") 
			}
		} else {
			message("File not available locally. Use 'download = TRUE'")
		}
	}	
	if (file.exists(filename)) {
		x <- readRDS(filename)
		# avoid pesky warnings
		if (type != 'sf') {
			crs(x) <- "+proj=longlat +datum=WGS84"
		}
		return(x)
	} else {
		return(NULL)
	}
}




.countries <- function(download, path, type='sp', ...) {

	if (type == 'sf') {
		f <- "countries_gadm36_sf.rds"
	} else {
		f <- "countries_gadm36_sp.rds"	
	}
	filename <- file.path(path, f)
	
	if (!file.exists(filename)) {
		if (download) {
			theurl <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/", f)
			.download(theurl, filename)
			if (!file.exists(filename)) {
				message("\nCould not download file -- perhaps it does not exist") 
			}
		} else {
			message("File not available locally. Use 'download = TRUE'")
		}
	}	
	if (file.exists(filename)) {
		#thisenvir = new.env()
		#data <- get(load(filename, thisenvir), thisenvir)
		data <- readRDS(filename)
		crs(data) <- "+proj=longlat +datum=WGS84"
		return(data)
	} 
}


.cmip5 <- function(var, model, rcp, year, res, lon, lat, path, download=TRUE) {
	if (!res %in% c(0.5, 2.5, 5, 10)) {
		stop('resolution should be one of: 2.5, 5, 10')
	}
	if (res==2.5) { 
		res <- '2_5m' 
    } else if (res == 0.5) {
        res <- "30s"
    } else {
		res <- paste(res, 'm', sep='')
	}
	
	var <- tolower(var[1])
	vars <- c('tmin', 'tmax', 'prec', 'bio')
	stopifnot(var %in% vars)
	var <- c('tn', 'tx', 'pr', 'bi')[match(var, vars)]
	
	model <- toupper(model)
	models <- c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG', 'HE', 'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
	stopifnot(model %in% models)
	
	rcps <- c(26, 45, 60, 85)
	stopifnot(rcp %in% rcps)
	stopifnot(year %in% c(50, 70))
	
	#m <- matrix(c(0,1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)
	
	m <- matrix(c(0,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,0,1,1,1,0,1,0,1,1,1,1,0,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)

	i <- m[which(model==models), which(rcp==rcps)]
	if (!i) {
		warning('this combination of rcp and model is not available')
		return(invisible(NULL))
	}
	
	path <- paste(path, '/cmip5/', res, '/', sep='')
	dir.create(path, recursive=TRUE, showWarnings=FALSE)

	zip <- tolower(paste(model, rcp, var, year, '.zip', sep=''))
	theurl <- paste('https://biogeo.ucdavis.edu/data/climate/cmip5/', res, '/', zip, sep='')

	zipfile <- paste(path, zip, sep='')
	if (var == 'bi') {
		n <- 19
	} else {
		n <- 12
	}
	tifs <- paste(extension(zip, ''), 1:n, '.tif', sep='')
	files <- paste(path, tifs, sep='')
	fc <- sum(file.exists(files))
	if (fc < n) {
		if (!file.exists(zipfile)) {
			if (download) {
				.download(theurl, zipfile)
				if (!file.exists(zipfile))	{ 
					message("\n Could not download file -- perhaps it does not exist") 
				}
			} else {
				message("File not available locally. Use 'download = TRUE'")
			}
		}	
		utils::unzip(zipfile, exdir=dirname(zipfile))
	}
	stack(paste(path, tifs, sep=''))
}

#.cmip5(var='prec', model='BC', rcp=26, year=50, res=10, path=getwd())

.cmip6 <- function(var, model, ssp, year, res, lon, lat, path, download=TRUE) {
  if (!res %in% c(2.5, 5, 10)) {
    stop('resolution should be one of: 2.5, 5, 10')
  }
  res <- paste0(res, "m")
  models <- c(
    "BCC-CSM2-MR",
    "CNRM-CM6-1",
    "CNRM-ESM2-1",
    "CanESM5",
    "GFDL-ESM4",
    "IPSL-CM6A-LR",
    "MIROC-ES2L",
    "MIROC6",
    "MRI-ESM2-0"
  )
  
  stopifnot(model %in% models)
  
  ssps <- c(126, 245, 370, 585)
  
  stopifnot(ssp %in% ssps)
  
  var <- tolower(var[1])
  vars <- c('tmin', 'tmax', 'prec', 'bio', 'bioc')
  stopifnot(var %in% vars)
  #for consistency with .cmip5:
  if(var == "bio") var <- "bioc"
  
  
  stopifnot(year %in% c(40, 60, 80, 100))
  
  year <- year + 2000
  yrange <- paste(year-19, year, sep = "-")
  
  #TODO check for combinations that aren't available
  
  #creat download dir
  path <- paste0(path, '/cmip6/', res, '/')
  dir.create(path, recursive=TRUE, showWarnings=FALSE)
  
  baseurl <- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/"
  zip <- paste0("wc2.1_", res, "_", var, "_", model, "_ssp", ssp, "_", yrange, ".zip")
  theurl <- paste0(baseurl, res, "/", zip)
  
  zipfile <- paste(path, zip, sep='')
  
  if (!file.exists(zipfile)) {
    if (download) {
      .download(theurl, zipfile)
      if (!file.exists(zipfile))	{ 
        message("\n Could not download file -- perhaps it does not exist") 
      }
    } else {
      message("File not available locally. Use 'download = TRUE'")
    }
  }	
  #tif file is buried deep, so unzip to tempdir and move .tif to top level
  utils::unzip(zipfile, exdir=tempdir())
  tiffile <- list.files(path = tempdir(),
                        pattern = extension(zip, ".tif"),
                        recursive = TRUE,
                        full.names = TRUE)
  outfile <- paste(dirname(zipfile), basename(tiffile), sep ="/")
  file.copy(from = tiffile, to = outfile)
  raster(outfile)
}


.worldclim <- function(var, res, lon, lat, path, download=TRUE) {
	if (!res %in% c(0.5, 2.5, 5, 10)) {
		stop('resolution should be one of: 0.5, 2.5, 5, 10')
	}
	if (res==2.5) { res <- '2-5' }
	stopifnot(var %in% c('tmean', 'tmin', 'tmax', 'prec', 'bio', 'alt'))
	path <- paste(path, 'wc', res, '/', sep='')
	dir.create(path, showWarnings=FALSE)

	if (res==0.5) {
		lon <- min(180, max(-180, lon))
		lat <- min(90, max(-60, lat))
		rs <- raster(nrows=5, ncols=12, xmn=-180, xmx=180, ymn=-60, ymx=90 )
		row <- rowFromY(rs, lat) - 1
		col <- colFromX(rs, lon) - 1
		rc <- paste(row, col, sep='') 
		zip <- paste(var, '_', rc, '.zip', sep='')
		zipfile <- paste(path, zip, sep='')
		if (var  == 'alt') {
			bilfiles <- paste(var, '_', rc, '.bil', sep='')
			hdrfiles <- paste(var, '_', rc, '.hdr', sep='')			
		} else if (var  != 'bio') {
			bilfiles <- paste(var, 1:12, '_', rc, '.bil', sep='')
			hdrfiles <- paste(var, 1:12, '_', rc, '.hdr', sep='')
		} else {
			bilfiles <- paste(var, 1:19, '_', rc, '.bil', sep='')
			hdrfiles <- paste(var, 1:19, '_', rc, '.hdr', sep='')		
		}
		theurl <- paste('https://biogeo.ucdavis.edu/data/climate/worldclim/1_4/tiles/cur/', zip, sep='')
	} else {
		zip <- paste(var, '_', res, 'm_bil.zip', sep='')
		zipfile <- paste(path, zip, sep='')
		if (var  == 'alt') {
			bilfiles <- paste(var, '.bil', sep='')
			hdrfiles <- paste(var, '.hdr', sep='')			
		} else if (var  != 'bio') {
			bilfiles <- paste(var, 1:12, '.bil', sep='')
			hdrfiles <- paste(var, 1:12, '.hdr', sep='')
		} else {
			bilfiles <- paste(var, 1:19, '.bil', sep='')
			hdrfiles <- paste(var, 1:19, '.hdr', sep='')	
		}
		theurl <- paste('https://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/', zip, sep='')
	}
	files <- c(paste(path, bilfiles, sep=''), paste(path, hdrfiles, sep=''))
	fc <- sum(file.exists(files))
	
	
	if ( fc < length(files) ) {
		if (!file.exists(zipfile)) {
			if (download) {
				.download(theurl, zipfile)
				if (!file.exists(zipfile))	{ 
					message("\n Could not download file -- perhaps it does not exist") 
				}
			} else {
				message("File not available locally. Use 'download = TRUE'")
			}
		}	
		utils::unzip(zipfile, exdir=dirname(zipfile))
		for (h in paste(path, hdrfiles, sep='')) {
			x <- readLines(h)
			x <- c(x[1:14], 'PIXELTYPE     SIGNEDINT', x[15:length(x)])
			writeLines(x, h)
		}
	}
	if (var  == 'alt') {
		st <- raster(paste(path, bilfiles, sep=''))
	} else {
		st <- stack(paste(path, bilfiles, sep=''))
	}
	projection(st) <- "+proj=longlat +datum=WGS84"
	return(st)
}



.raster <- function(country, name, mask=TRUE, path, download, keepzip=FALSE, ...) {

	country <- .getCountry(country)
	path <- .getDataPath(path)
	if (mask) {
		mskname <- '_msk_'
		mskpath <- 'msk_'
	} else {
		mskname<-'_'
		mskpath <- ''		
	}
	filename <- paste(path, country, mskname, name, ".grd", sep="")
	if (!file.exists(filename)) {
		zipfilename <- filename
		extension(zipfilename) <- '.zip'
		if (!file.exists(zipfilename)) {
			if (download) {
				theurl <- paste("https://biogeo.ucdavis.edu/data/diva/", mskpath, name, "/", country, mskname, name, ".zip", sep="")
				.download(theurl, zipfilename)
				if (!file.exists(zipfilename))	{ 
					message("\nCould not download file -- perhaps it does not exist") 
				}
			} else {
				message("File not available locally. Use 'download = TRUE'")
			}
		}
		ff <- utils::unzip(zipfilename, exdir=dirname(zipfilename))
		if (!keepzip) {
			file.remove(zipfilename)
		}
	}	
	if (file.exists(filename)) { 
		rs <- raster(filename)
	} else {
		#patrn <- paste(country, '.', mskname, name, ".grd", sep="")
		#f <- list.files(path, pattern=patrn)
		f <- ff[substr(ff, nchar(ff)-3, nchar(ff)) == '.grd']
		if (length(f)==0) {
			warning('something went wrong')
			return(NULL)
		} else if (length(f)==1) {
			rs <- raster(f)
		} else {
			rs <- sapply(f, raster)
			message('returning a list of RasterLayer objects')
			return(rs)
		}
	}
	projection(rs) <- "+proj=longlat +datum=WGS84"
	return(rs)	
}



.SRTM <- function(lon, lat, download, path) {
	stopifnot(lon >= -180 & lon <= 180)
	stopifnot(lat >= -60 & lat <= 60)
	
	rs <- raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
	rowTile <- rowFromY(rs, lat)
	colTile <- colFromX(rs, lon)
	if (rowTile < 10) { rowTile <- paste('0', rowTile, sep='') }
	if (colTile < 10) { colTile <- paste('0', colTile, sep='') }

	baseurl <- "https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/"
	
	f <- paste0("srtm_", colTile, "_", rowTile, ".zip")

	zipfilename <- file.path(path, f)
	tiffilename <- file.path(path, gsub(".zip$", ".tif", f))
	
	if (!file.exists(tiffilename)) {
		if (!file.exists(zipfilename)) {
			if (download) { 
				theurl <- paste0(baseurl, f)
				test <- try (.download(theurl, zipfilename) , silent=TRUE)
				if (inherits(test, "try-error")) {
					stop("cannot download the file")
				}
			} else {message("file not available locally, use download=TRUE") }	
		} 
		if (file.exists(zipfilename)) { 
			utils::unzip(zipfilename, exdir=dirname(zipfilename))
			file.remove(zipfilename)
		}	
	}
	if (file.exists(tiffilename)) { 
		rs <- raster(tiffilename)
		projection(rs) <- "+proj=longlat +datum=WGS84"
		return(rs)
	} else {
		stop('file not found')
	}
}

#.SRTM(lon=5.5, lat=44.5, TRUE, ".")
