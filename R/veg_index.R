#' Compute the NDVI index
#'
#' NDVI = (NIR - RED) / (NIR + RED)
#'
#' @export
#'
ndvi <- function(x) {
  z <- (x[[5]] - x[[3]]) / (x[[5]] + x[[3]])
  return(z)
}

#' Compute the NDVI index using red edge band
#'
#' NDVI_RE = (NIR - Red Edge) / (NIR + Red Edge)
#'
#' @export
#'
ndvi_re <- function(x) {
  z <- (x[[5]] - x[[4]]) / (x[[5]] + x[[4]])
  return(z)
}

#' Compute the VARI index using red edge band
#'
#' VARi_RE = (Red Edge –  RED) / (Red Edge + RED)
#'
#' @export
#'
vari_re <- function(x) {
  z <- (x[[4]] - x[[3]]) / (x[[4]] + x[[3]])
  return(z)
}

#' Compute the CI index
#'
#' CI_g = (NIR / GREEN) – 1
#'
#' @export
#'
ci <- function(x) {
  z <- (x[[5]] / x[[2]]) - 1
  return(z)
}

#' Compute the CI index using red edge band
#'
#' CI_g = (NIR / Red Edge) – 1
#'
#' @export
#'
ci_re <- function(x) {
  z <- (x[[5]] / x[[4]]) - 1
  return(z)
}

#' Compute the MTCI index
#'
#' MTCI = (NIR - Red Edge) / (Red Edge - RED)
#'
#' @export
#'
mtci <- function(x) {
  z <- (x[[5]] - x[[4]]) / (x[[4]] - x[[3]])
  return(z)
}

#' Compute the SR index
#'
#' @export
#'
sr <- function(x) {
  z <- x[[5]] / x[[3]]

  return(z)
}

#' Compute the SR index using red edge band
#'
#' @export
#'
sr_re <- function(x) {
  z <- x[[5]] / x[[4]]

  return(z)
}
