context("Reading netCDF")

#create a temporary netCDF file
r <- raster::brick(array(1:(10*15*5), dim= c(10, 15, 5)), crs = CRS("+proj=longlat +datum=WGS84"))
#the array dimension of length 10 becomes latitude, and is represented as rows in the data matrix of r
#the array dimension of length 15 becomes longitude, and is represented as columns in the data matrix of r
tmpfilename <- tempfile()
tmpfilename <- paste0(tmpfilename, ".nc")
writeRaster(r, tmpfilename)

#preparing comparison values
testpt1 <- SpatialPoints(matrix(c(0.0912,0.30001), ncol = 2, byrow = TRUE), proj4string = CRS(proj4string(r)))
testpt1_vals <- extract(r, testpt1)
testpt1r <- SpatialPoints(matrix(c(1 - 0.30001, 0.0912), ncol = 2, byrow = TRUE), proj4string = CRS(proj4string(r)))
testpt1rr <- SpatialPoints(matrix(c(4, 0.0912), ncol = 2, byrow = TRUE), proj4string = CRS(proj4string(r)))

test_that("netCDF file is correctly read with dimension order (2, 1, 3)", {
  b <- raster::brick(tmpfilename, dims = c(2, 1, 3))
  expect_equal(dim(b), c(15, 10, 5))  #latitude (rows in r's matrix) become columns in b, longitude (cols in r's matrix) becomes rows in b
  expect_equal(dim(as.matrix(raster(b, 1))), dim(as.matrix(raster(r, 1)))[c(2, 1)])
  expect_equal(dim(getValues(b, 1, 2)), c(20, 5))
  vals <- extract(b, testpt1r)
  expect_equivalent(vals, testpt1_vals)
})

test_that("netCDF file is correctly read with dimension order (3, 1, 2)", {
  b <- raster::brick(tmpfilename, dims = c(3, 1, 2))
  expect_equal(dim(b), c(15, 5, 10)) #longitude (cols in r's matrix) becomes rows in b, layers becomes cols in b, latitude becomes layers
  expect_equal(dim(as.matrix(raster(b, 1))), c(15, 5))
  val <- extract(b, testpt1rr)[10 - 3]  #10 -3 is the layer corresponding to 0.3 (I think?)
  expect_equivalent(val, testpt1_vals[4])
})

# WARNING: the following errors in both situations above
# getValues(b, 1, 1)

unlink(tmpfilename)


############################### 2 dimensional file
#create a temporary netCDF file
r <- raster::raster(array(1:(10*15), dim= c(10, 15)), crs = CRS("+proj=longlat +datum=WGS84"))
#the array dimension of length 10 becomes latitude, and is represented as rows in the data matrix of r
#the array dimension of length 15 becomes longitude, and is represented as columns in the data matrix of r
tmpfilename <- tempfile()
tmpfilename <- paste0(tmpfilename, ".nc")
writeRaster(r, tmpfilename)

#preparing comparison values
testpt1 <- SpatialPoints(matrix(c(0.0912,0.30001), ncol = 2, byrow = TRUE), proj4string = CRS(proj4string(r)))
testpt1_vals <- extract(r, testpt1)
testpt1r <- SpatialPoints(matrix(c(1 - 0.30001, 0.0912), ncol = 2, byrow = TRUE), proj4string = CRS(proj4string(r)))

test_that("netCDF file is correctly read with dimension order (2, 1)", {
  b <- raster::raster(tmpfilename, dims = c(2, 1))
  expect_equal(dim(b), c(15, 10, 1))  #latitude (rows in r's matrix) become columns in b, longitude (cols in r's matrix) becomes rows in b
  expect_equal(dim(as.matrix(b)), dim(as.matrix(r))[c(2, 1)])
  expect_equal(length(getValues(b, 1)), 10)
  vals <- extract(b, testpt1r)
  expect_equivalent(vals, testpt1_vals)
})

unlink(tmpfilename)