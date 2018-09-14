
loadModule("spmod", TRUE)

#.onLoad <- function(lib, pkg)  {
#	pkg.info <- utils::packageDescription('raster') 
#	packageStartupMessage(paste("raster ", pkg.info[["Version"]], " (", pkg.info["Date"], ")", sep=""))
#	wd <- getwd()
#	options('startup.working.directory'=wd)
#	fn <- paste(wd, '/rasterOptions_', pkg.info[["Version"]], sep='')
#	.loadOptions(fn)

#	try( removeTmpFiles( .tmptime() ), silent=TRUE ) 
#	return(invisible(0))
#}

