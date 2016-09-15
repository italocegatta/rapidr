#' Get the Scale Factor for all bands
#'
#' @param xml An XML document
#'
#' @return A vector contain the scale factor of each 5 bands
#'
#' @examples
#' library(xml2)
#'
#' xml.file <- list.files(
#'   system.file("extdata", package="rapidr"),
#'   pattern = ".xml", full.names = TRUE
#' )
#'
#' x <- read_xml(xml.file)
#'
#' rapid_sf(x)
#'
#' @export
#'
rapid_sf <- function(xml) {
  z <- xml %>%
    xml2::xml_find_all("///re:radiometricScaleFactor") %>%
    xml2::xml_text() %>%
    as.double()

  return(z)
}

#' Get the acquisition date
#'
#' @param xml An XML document
#'
#' @return A class Date
#'
#' @examples
#' library(xml2)
#'
#' xml.file <- list.files(
#'   system.file("extdata", package="rapidr"),
#'   pattern = ".xml", full.names = TRUE
#' )
#'
#' x <- read_xml(xml.file)
#'
#' rapid_date(x)
#'
#' @export
#'
rapid_date <- function(xml) {
  z <- xml %>%
    xml2::xml_find_all("////re:acquisitionDateTime") %>%
    xml2::xml_text() %>%
    stringr::str_extract("[:digit:]+-[:digit:]+-[:digit:]+") %>%
    lubridate::ymd()

  return(z)
}

#' Get the acquisition hour
#'
#' @param xml An XML document
#'
#' @return A decimal hour
#'
#' @examples
#' library(xml2)
#'
#' xml.file <- list.files(
#'   system.file("extdata", package="rapidr"),
#'   pattern = ".xml", full.names = TRUE
#' )
#'
#' x <- read_xml(xml.file)
#'
#' rapid_hour(x)
#'
#' @export
#'
rapid_hour <- function(xml) {
  z <- xml %>%
    xml2::xml_find_all("////re:acquisitionDateTime") %>%
    xml2::xml_text() %>%
    lubridate::ymd_hms() %>%
    {
      lubridate::hour(.) +
      lubridate::minute(.) / 60 +
      lubridate::second(.) / 3600
    }

  return(z)
}

#' Get the EPSG reference coordinate
#'
#' @param xml An XML document
#'
#' @return A SpatialPoints of central coordinates from Rapideye tile
#'
#' @examples
#' library(xml2)
#'
#' xml.file <- list.files(
#'   system.file("extdata", package="rapidr"),
#'   pattern = ".xml", full.names = TRUE
#' )
#'
#' x <- read_xml(xml.file)
#'
#' rapid_epsg(x)
#'
#' @export
#'
rapid_epsg <- function(xml) {
  z <- xml %>%
    xml2::xml_find_all("///re:epsgCode") %>%
    xml2::xml_text() %>%
    as.numeric()

  return(z)
}

#' Get the central latitude and longitude of the tile
#'
#' @param xml An XML document
#'
#' @return A SpatialPoints of central coordinates from Rapideye tile
#'
#' @examples
#' library(xml2)
#'
#' xml.file <- list.files(
#'   system.file("extdata", package="rapidr"),
#'   pattern = ".xml", full.names = TRUE
#' )
#'
#' x <- read_xml(xml.file)
#'
#' rapid_latlon(x)
#'
#' @export
#'
rapid_latlon <- function(xml) {
  xml %>%
    xml2::xml_find_all("//gml:pos") %>%
    xml2::xml_text() %>%
    stringr::str_split("\\s+") %>%
    unlist() %>%
    as.double()
}
