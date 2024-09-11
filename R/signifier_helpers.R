
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

calculate_triad_means <- function(data, sig_id, mean_type, framework_object) {

  stopifnot(framework_object$get_signifier_type(sig_id) == "triad")
  stopifnot(is.data.frame(data))
  stopifnot(mean_type %in% c("geometric", "arithmetic"))

  triad_col_names <- framework_object$get_triad_anchor_column_names(sig_id)


  if (mean_type == "geometric") {

    data_left <- ifelse(data[[triad_col_names[["left"]]]] == 0, 0.00001, data[[triad_col_names[["left"]]]])
    data_top <- ifelse(data[[triad_col_names[["top"]]]] == 0, 0.00001, data[[triad_col_names[["top"]]]])
    data_right <- ifelse(data[[triad_col_names[["right"]]]] == 0, 0.00001, data[[triad_col_names[["right"]]]])

    geom_mean <- data.frame(data.table::transpose(data.frame(compositions::clo(c(compositions::geometricmean(data_left[!is.na(data_left)]),
                                                             compositions::geometricmean(data_top[!is.na(data_top)]),
                                                             compositions::geometricmean(data_right[!is.na(data_right)])), total = 100))))

    colnames(geom_mean) <- c("x", "y", "z")
    left_mean <- round(geom_mean[[1]], digits = 0)
    top_mean <- round(geom_mean[[2]], digits = 0)
    right_mean <- round(geom_mean[[3]], digits = 0)

  } else {
    if (mean_type == "arithmetic") {
    left_mean <- round(mean(data[!is.na(data[[triad_col_names[["left"]]]]), triad_col_names[["left"]]], na.rm = TRUE), digits = 0)
    top_mean <- round(mean(data[!is.na(data[[triad_col_names[["top"]]]]), triad_col_names[["top"]]], na.rm = TRUE), digits = 0)
    right_mean <- round(mean(data[!is.na(data[[triad_col_names[["right"]]]]), triad_col_names[["right"]]], na.rm = TRUE), digits = 0)
    }
  }

  return(list(left_mean = left_mean, top_mean = top_mean, right_mean = right_mean))
}

get_anchor_plot_titles <- function(sig_id, display_anchor_means, anchor_means, framework_object) {

  left_title <- clearBetweenHTMLTags(paste(stringr::str_replace_all(framework_object$get_triad_left_anchor_text(sig_id), "&amp;", "&"),  ifelse(display_anchor_means,  paste("\U003BC", "=", anchor_means[["left_mean"]]), "")), " ")
  right_title <-  clearBetweenHTMLTags(paste(stringr::str_replace_all(framework_object$get_triad_right_anchor_text(sig_id), "&amp;", "&"), ifelse(display_anchor_means, paste("\U003BC", "=", anchor_means[["right_mean"]]), "")), " ")
  top_title <- clearBetweenHTMLTags(paste(stringr::str_replace_all(framework_object$get_triad_top_anchor_text(sig_id), "&amp;", "&"),  ifelse(display_anchor_means, paste("\U003BC","=", anchor_means[["top_mean"]]), "")), " ")


  if (!(grepl("\n", left_title, fixed = TRUE)  || grepl("\n", right_title, fixed = TRUE))) {
    if (stringr::str_detect(left_title, "\\s")) {
      if (nchar(left_title) > 23) {
        if (is.na(stringr::str_locate(stringr::str_sub(left_title, round(nchar(left_title) / 2, digits = 0), nchar(left_title)), " ")[[1]])) {
          stringi::stri_sub(left_title, stringi::stri_locate_last_fixed(left_title, " ") + 1, 1) <- "\n"
        } else {
          stringi::stri_sub(left_title, stringr::str_locate(stringr::str_sub(left_title, round(nchar(left_title) / 2, digits = 0), nchar(left_title)), " ")[[1]] + round(nchar(left_title) / 2, digits = 0), 1) <- "\n"
        }
      }
    }


    if (stringr::str_detect(right_title, "\\s")) {
      if (nchar(right_title) > 23) {
        if (is.na(stringr::str_locate(stringr::str_sub(right_title, round(nchar(right_title) / 2, digits = 0), nchar(right_title)), " ")[[1]])) {
          stringi::stri_sub(right_title, stringi::stri_locate_last_fixed(right_title, " ") + 1, 1) <- "\n"
        } else {
          stringi::stri_sub(right_title, stringr::str_locate(stringr::str_sub(right_title, round(nchar(right_title) / 2, digits = 0), nchar(right_title)), " ")[[1]] + round(nchar(right_title) / 2, digits = 0), 1) <- "\n"
        }
      }
    }
  }

  return(list(left_title = left_title, right_title = right_title, top_title = top_title))

  }

get_anchor_plot_size <- function (anchor_titles) {

  left_title <- anchor_titles[["left_title"]]
  right_title <-  anchor_titles[["right_title"]]
  top_title <- anchor_titles[["top_title"]]

  titleLength <- max(c(nchar(left_title), nchar(right_title)))

  anchor_size <- 4.5
  if (titleLength > 25) {
    anchor_size <- 4.0
  }
  if (titleLength > 50) {
    anchor_size <- 3.5
  }
  if (titleLength > 55) {
    anchor_size <- 3.0
  }
  if (titleLength > 65) {
    anchor_size <- 2.8
  }
  if (titleLength > 90) {
    anchor_size <- 2.0
  }

  return(anchor_size)

}

get_graph_title_size <- function(title, title_size_multiplier = 1) {

  title_size <- 18
  if (is.null(title)) {return(title_size)}
  if (nchar(title) > 55) {title_size <- 16 * title_size_multiplier}
  if (nchar(title) > 65) {title_size <- 15 * title_size_multiplier}
  if (nchar(title) > 80) {title_size <- 14 * title_size_multiplier}
  if (nchar(title) > 95) {title_size <- 13 * title_size_multiplier}
  if (nchar(title) > 120) {title_size <- 10 * title_size_multiplier}
  return(title_size)
}



get_caption_values <- function(filtered_data, full_data, sig_id, framework_object) {

  naAllowed <- framework_object$get_signifier_allow_na(sig_id)
  numNADataPoints <- 0
  numNADataPointsMu <- 0
  if (naAllowed) {
    colNAName <- framework_object$get_triad_na_column_name(sig_id)
    if (colNAName %in% names(full_data)) {
      numNADataPointsMu <- length(full_data[!is.na(full_data[,colNAName]),colNAName])
      numNADataPoints <- length(filtered_data[!is.na(filtered_data[,colNAName]),colNAName])
    }
  }

  colLeftName <- framework_object$get_triad_anchor_column_names(sig_id)[["left"]]
  numDataPointsMu <- length(full_data[!is.na(full_data[,colLeftName]) , colLeftName])

  numDataPoints <- length(filtered_data[!is.na(filtered_data[,colLeftName]) , colLeftName])

  numNonEntries <- nrow(filtered_data) - (numDataPoints + numNADataPoints)
  perToData <- round((numDataPoints / numDataPointsMu) * 100, digits = 0)

  return(list(N = nrow(full_data),  numDataPointsMu = numDataPointsMu, numNADataPointsMu = numNADataPointsMu, numNonEntries = numNonEntries, numNADataPoints = numNADataPoints, perToData = perToData ))

}
