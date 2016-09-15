#' Extract a zonal statistics by polygon
#'
#' @export
#'
extract2df <- function(raster, extent, fun = mean) {
  e <- raster::extract(raster, extent, fun = fun, na.rm = TRUE, sp = TRUE)
  z <- as.data.frame(e)

  return(z)
}
