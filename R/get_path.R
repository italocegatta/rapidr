#' Get the path of .tif files
#'
#' @export
#'
path_tif <- function(path) {
  list.files(
    path = path, pattern = ".tif$",
    full.names = T, recursive = T
  ) %>%
    "["(!str_detect(., "(browse|udm)"))
}

#' Get the path of .img files
#'
#' @export
#'
path_img <- function(path) {
  list.files(
    path = path, pattern = ".img$",
    full.names = T, recursive = T
  )
}

#' Get the path of .shp files
#'
#' @export
#'
path_shp <- function(path) {
  list.files(
    path = path, pattern = ".shp$",
    full.names = T, recursive = T
  )
}
