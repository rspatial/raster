context("test-sf-coercion")

p1 <- structure(cbind(0, 0), class = c("XY", "POINT", "sfg"))
p2 <- structure(cbind(1, 1), class = c("XY", "POINT", "sfg"))
sf <- structure(data.frame(a = 1:2, geometry = structure(list(p1, p2), class = c("sfc_POINT", "sfc"), 
                                        bbox = structure(c(xmin = 0, ymin = 0, xmax = 1, ymax = 1), class = "bbox"), 
                                                         crs = structure(list(epsg = NA_integer_, proj4string = NA_character_), class = "crs"), precision = 0)), 
                class = c("sf", "data.frame"), sf_column = "geometry", agr = factor(NA, c("constant", "aggregate", "identity")))

                                                                             

test_that("raster from sf works", {
  expect_that(raster(sf), is_a("RasterLayer"))
  expect_that(brick(sf),  is_a("RasterBrick"))
  expect_that(stack(sf,   is_a("RasterStack")))
})


