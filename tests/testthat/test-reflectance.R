
context("Calculate reflectance")

xml.file <- list.files(
  system.file("extdata", package="rapidr"),
  pattern = ".xml", full.names = T
)

raster.file <- list.files(
  system.file("extdata", package="rapidr"),
  pattern = ".tif", full.names = T
)

x <- xml2::read_xml(xml.file)
r <- raster::brick(raster.file)

test_that("rapid_reflec works as expected",{
  expect_equal(
    as.vector(raster::extract(rapid_reflec(r, x), cbind(493507.817, 4439928.777))),
    c(0.208910301, 0.188695326, 0.213532568, 0.263995584, 0.341353726),
    tolerance = 0.001
  )
})
