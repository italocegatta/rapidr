#' Crop and mask a raster by polygon
#'
#' @export
#'
clip_raster <- function(r, s) {
  a <- raster::crop(r, s, snap="in")
  z <- raster::mask(a, s)

  return(z)
}
