context("Get informations")

library(xml2)

xml.file <- list.files(
  system.file("extdata", package="rapidr"),
  pattern = ".xml", full.names = T
)
x <- read_xml(xml.file)

test_that("rapid_sf works as expected",{
  expect_equal(rapid_sf(x), rep(9.999999776482582e-03, 5))
})

test_that("rapid_date works as expected",{
  expect_equal(rapid_date(x), as.Date("2012-07-21"))
})

test_that("rapid_hour works as expected",{
  expect_equal(rapid_hour(x), 18.99322328)
})

test_that("rapid_epsg works as expected",{
  expect_equal(rapid_epsg(x), 32613)
})

test_that("rapid_latlon works as expected",{
  expect_equal(rapid_latlon(x), c(4.000207e+01, -1.051406e+02))
})

