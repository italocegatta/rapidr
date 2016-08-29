# day of the year
#
doy <- function(date) {
  z <- as.POSIXlt(date)$yday + 1
  return(z)
}

# solar declination
#
declin <- function(doy) {
  z <- asin(
    0.39785 * sin(4.869 + (0.0172 * doy) + 0.03345 * sin(6.224 + 0.0172 * doy))
    )
  return(z)
}

# time equation
#
te <- function(doy) {
  a <- (279.575 + 0.986 * doy) * (pi / 180)
  z <- (
    -104.7 * sin(a) + 596.2 * sin(2 * a) + 4.3 * sin(3 * a) - 12.7 * sin(4 *
                                                                           a) -
      429.9 * cos(a) - 2 * cos(2 * a) + 19.3 * cos(3 * a)
  ) / 3600
  return(z)
}

# minimum solar zenith
#
to <- function(lon, te) {
  lc <- (lon - (-45)) * (1 / 15)
  z <- 12 - lc - te
  return(z)
}

# zenith angle
#
zh <- function(lat, declin, hour, to) {
  z <- acos((sin(lat * pi / 180) * sin(declin)) +
              (cos(lat * pi / 180) * cos(declin) * cos(0.2618 * (hour - to)))) *
    (180 / pi)
  return(z)
}

# Earth–Sun distance
#
# reference https://www.researchgate.net/file.PostFileLoader.html?id=5635f25060614b180d8b4567&assetKey=AS%3A290936253894656%401446376016229
#
sundist2 <- function(doy) {
  (1 + 0.033 * cos(2 * pi * doy / 365 * pi / 180)) ^ -1
}
