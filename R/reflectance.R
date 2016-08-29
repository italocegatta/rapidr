#' Convert the Digital Number from raw image to Reflectance
#'
#' @param raster A RasterBrick object
#' @param xml An XML document
#'
#' @return A RasterBrick object
#'
#' @examples
#' library(raster)
#' library(xml2)
#'
#' xml.file <- list.files(
#'   system.file("extdata", package="rapidr"),
#'   pattern = ".xml", full.names = TRUE
#' )
#' raster.file <- list.files(
#'   system.file("extdata", package="rapidr"),
#'   pattern = ".tif", full.names = TRUE
#' )
#' x <- read_xml(xml.file)
#' r <- raster::brick(raster.file)
#'
#' rapid_reflec(r, x)
#'
#' @export
#'
rapid_reflec <- function(raster, xml) {
  .sf <- rapid_sf(xml)
  .doy <- doy(rapid_date(xml))
  .sundist2 <- sundist2(.doy)
  .lat <- rapid_latlon(xml)[1]
  .lon <- rapid_latlon(xml)[2]
  .zn <- zh(
    .lat, declin(.doy), rapid_hour(xml),
    to(.lon, te(.doy))
  )

  b1 <- raster[[1]] * .sf[1] * pi * .sundist2 / (1560.4 * cos(.zn * pi / 180))
  b2 <- raster[[2]] * .sf[2] * pi * .sundist2 / (1863.5 * cos(.zn * pi / 180))
  b3 <- raster[[3]] * .sf[3] * pi * .sundist2 / (1997.8 * cos(.zn * pi / 180))
  b4 <- raster[[4]] * .sf[4] * pi * .sundist2 / (1395.0 * cos(.zn * pi / 180))
  b5 <- raster[[5]] * .sf[5] * pi * .sundist2 / (1124.4 * cos(.zn * pi / 180))

  z <- raster::brick(b1, b2, b3, b4, b5)

  return(z)
}
