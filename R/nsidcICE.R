.rasterFromNSIDCFile <- function(x) {
    ## check name structure
    ## "nt_19781119_f07_v01_s.bin"

    bx <- basename(x)
    ## test that we can get a date from this
    ## (as POSIXct so that Z-comparisons are more natural)
    dts <- as.POSIXct(basename(x), format = "nt_%Y%m%d", tz = "GMT")
    ## test that we see _f and _v
    fyes <- tolower(substr(bx, 13L, 13L)) %in% c("f", "n")
    vyes <- tolower(substr(bx, 17L, 17L)) %in% c("v", "n")

    ## finally, it's north or south
    hemi <- tolower(substr(bx, 21L, 21L))
    hyes <- hemi %in% c("s", "n")
    if(!(!is.na(dts) & fyes & vyes & hyes)) return(NULL)

    ## NSIDC projection and grid size
    ## https://nsidc.org/data/polar_stereo/ps_grids.html
    ## http://spatialreference.org/ref/?search=nsidc
    ## Hughes 1980 ellipsoid, True Scale Lat is +/-70

    if (hemi == "s") {
        prj <-  "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"

        dims <- c(316L, 332L)
        ext <- c(-3950000, 3950000, -3950000, 4350000)
    } else {
        ## northern hemisphere
        prj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
        dims <- c(304, 448)
        ext <- c(-3837500, 3762500, -5362500, 5837500)
    }
    on.exit(close(con))
    con <- file(x, open = "rb")

    ## chuck the header
    try1 <- try(trash <- readBin(con, "integer", size = 1, n = 300))
    ## TODO: warnings that we thought it was NSIDC, but it did not work?
    if (inherits(try1, "try-error")) return(NULL)
    dat <- try(readBin(con, "integer", size = 1, n = prod(dims), endian = "little", signed = FALSE))
    if (inherits(dat, "try-error")) return(NULL)

    r100 <- dat > 250
    r0 <- dat < 1
##      if (rescale) {
        dat <- dat/2.5  ## rescale back to 100
##      }
##      if (setNA) {
        dat[r100] <- NA
       ## dat[r0] <- NA
##      }
    r <- raster(t(matrix(dat, dims[1])), xmn=ext[1], xmx=ext[2], ymn=ext[3], ymx=ext[4], crs = prj)

    setZ(r, dts, name = "time")

}
