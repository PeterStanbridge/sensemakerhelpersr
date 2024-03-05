

#' get a triad value's zone
#'
#' @param top percentage triad top value (between 0 and 100).
#' @param left  percentage triad left value (between 0 and 100).
#' @param right  percentage triad right value (between 0 and 100).
#'
#' @returns A single character returning the triad zone
#' @export
#'
#' @examples
#' t <- 30
#' l <- 65
#' r <- 5
#' calculate_triad_zone(t, l, r)
calculate_triad_zone <- function(top, left, right) {


  if (any(is.na(c(top, left, right)))) {
    return(NA)
  }

  stopifnot(is.numeric(c(top,  left,  right)))
  stopifnot(sum(top, left, right, na.rm = TRUE) == 100) # na.rm strickly not necessary here
  stopifnot(all(c(top,  left,  right) >= 0 ) & all(c(top,  left,  right) <=  100))



  zone <- NULL
  if (left >= 60) {zone <- "L"}
  if (top >= 60) {zone <- "T"}
  if (right > 60) {zone <- "R"}
  if (left < 60 & left >= 20 & top < 60 & top >= 20 & right < 20) {zone <- "LT"}
  if (left <= 20 & top < 60 & top >= 20 & right <= 60 & right >= 20) {zone <- "TR"}
  if (left < 60 & left >= 20 & top < 20 & right <= 60 & right >= 20) {zone <- "LR"}
  if (left < 60 & left >= 20 & top >= 20 & top < 60 & right < 60 & right >= 20) {zone <- "Centre"}
  return(zone)
}

#' get a dyad value's zone
#'
#' @param left  percentage dyad left value (between 0 and 100).
#' @param right  percentage dyad right value (between 0 and 100). default 100 = left (enabling the pass of the left only)
#'
#' @returns A single character returning the zone
#' @export
#'
#' @examples
#' l <- 65
#' r <- 35
#' calculate_dyad_zone(l, r)
calculate_dyad_zone <- function(left, right = 100 - left) {


  if (any(is.na(c(left, right)))) {
    return(NA)
  }


  stopifnot(is.numeric(c(left,  right)))
  stopifnot(sum(left, right, na.rm = TRUE) == 100) # na.rm not strickly needed here
  stopifnot(all(c(left,  right) >= 0 ) & all(c(left,  right) <=  100))

  zone <- NULL
  if (left > 80) {zone <- "Right"}
  if (left > 60 & left <= 80) {zone <- "Centre-Right"}
  if (left > 40 & left <= 60) {zone <- "Centre"}
  if (left > 20 & left <= 40) {zone <- "Centre-Left"}
  if (left <= 20) {zone <- "Left"}
  return(zone)
}


#' get a stone value's x or y zone
#'
#' @param x  fraction stone x or y left value (between 0 and 1).
#'
#' @returns A single character returning the zone
#' @export
#'
#' @examples
#' l <- 0.5
#' calculate_stone_x_y_zone(l)
calculate_stone_x_y_zone <- function(x) {


  if (is.na(x)) {
    return(NA)
  }

  stopifnot(is.double(x))
  stopifnot(x >= 0 & x <=  1)

  zone <- NULL
  if (x > .8) {zone <- "Right"}
  if (x > .6 & x <= .8) {zone <- "Centre-Right"}
  if (x > .4 & x <= .6) {zone <- "Centre"}
  if (x > .2 & x <= .4) {zone <- "Centre-Left"}
  if (x <= .2) {zone <- "Left"}
  return(zone)
}

#' get a stone value's 4 zone
#'
#' @param x  fraction stone x value.
#'@param y  fraction stone y value.
#' @returns A character string returning the zone
#' @export
#'
#' @examples
#' l <- 0.7
#' t <- 0.3
#' calculate_stone_4_zone(l, t)
calculate_stone_4_zone = function(x, y) {


  if (any(is.na(c(x, y)))) {
    return(NA)
  }

  stopifnot(is.double(c(x, y)))
  stopifnot(x >= 0 & x <=  1)
  stopifnot(y >= 0 & y <=  1)
 # stopifnot(X + y == 1) ToDo find approx version

  zone <- NULL
    if (x >= 0.5 & y >= 0.5) {zone <- "Top_Right"}
    if (x >= 0.5 & y < 0.5) {zone <- "Bottom_Right"}
    if (x < 0.5 & y < 0.5) {zone <- "Bottom_Left"}
    if (x < 0.5 & y >= 0.5) {zone <- "Top_Left"}

    return(zone)
}



#' get a stone value's 9 zone
#'
#' @param x  fraction stone x value.
#'@param y  fraction stone y value.
#' @returns A character string returning the zone
#' @export
#'
#' @examples
#' l <- 0.7
#' t <- 0.3
#' calculate_stone_9_zone(l, t)
calculate_stone_9_zone = function(x, y) {


  if (any(is.na(c(x, y)))) {
    return(NA)
  }

  stopifnot(is.double(c(x, y)))
  stopifnot(x >= 0 & x <=  1)
  stopifnot(y >= 0 & y <=  1)

  zone <- NULL

  if (x >= 0.66666666 & y >= 0.66666666) {zone <- "Top_Right"}
  if (x >= 0.66666666 & y >= 0.33333333 & y < 0.66666666) {zone <- "Centre_Right"}
  if (x >= 0.66666666 & y < 0.33333333) {zone <- "Bottom_Right"}
  if (x >= 0.33333333 & x < 0.66666666 & y >= 0.66666666) {zone <- "Top_Centre"}
  if (x >= 0.33333333 & x < 0.66666666 & y >= 0.33333333 & y < 0.66666666) {zone <- "Centre"}
  if (x >= 0.33333333 & x < 0.66666666 &  y < 0.33333333) {zone <- "Bottom_Centre"}
  if (x < 0.33333333 & y >= 0.66666666) {zone <- "Top_Left"}
  if (x < 0.33333333 & y >= 0.33333333 & y < 0.66666666) {zone <- "Centre_Left"}
  if (x < 0.33333333 & y < 0.33333333) {zone <- "Bottom_Left"}

  return(zone)

}
