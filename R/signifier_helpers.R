
#' get a triad value's zone
#'
#' @title get a triad value's zone
#' @description
#' This function returns the zone value for a triad based on passing in a left, top and right compositional value.
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
#' @param y  fraction stone y value.
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
#' @param y  fraction stone y value.
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

#' @title Calculate triad means
#' @description
#' Calculate the triad means using specified approach
#' @param data - The data containing the triad top, left and right columns. This can be the full data dataframe, provided the triad's 3 columns are present.
#' @param triad_id - The signifier id of the triad to calculate means.
#' @param mean_type - either "geometric" or "arithmetic".
#' @param framework_object - The sensemakerframeworkr object associated with the framework.
#' @param zero_logic - either "small_value" or "remove". Small value means to take a zero value and give it a very small replacement. Otherwise ignore this fragment.
#' @param for_ternary - Default FALSE, if true, the returned dataframe containing the mean is in a format ready to plot in ggtern.
#' @returns The triad means as 3 values.
#' @export
calculate_triad_means <- function(data, triad_id, mean_type, framework_object, zero_logic = "small_value", for_ternary = FALSE) {

  stopifnot(framework_object$get_signifier_type(triad_id) == "triad")
  stopifnot(is.data.frame(data))
  stopifnot(mean_type %in% c("geometric", "arithmetic"))

  triad_col_names <- framework_object$get_triad_anchor_column_names(triad_id)


  if (mean_type == "geometric") {

    if (zero_logic == "small_value") {
    data_left <- ifelse(data[[triad_col_names[["left"]]]] <= 0, 0.00001, data[[triad_col_names[["left"]]]])
    data_top <- ifelse(data[[triad_col_names[["top"]]]] <= 0, 0.00001, data[[triad_col_names[["top"]]]])
    data_right <- ifelse(data[[triad_col_names[["right"]]]] <= 0, 0.00001, data[[triad_col_names[["right"]]]])
    } else {
      if (zero_logic == "remove") {
        remain_data <- data %>% dplyr::filter(is.na(.data[[triad_col_names[["left"]]]]) | .data[[triad_col_names[["left"]]]] != 0 &
                                                .data[[triad_col_names[["top"]]]] != 0 &
                                                .data[[triad_col_names[["right"]]]] != 0)
      }
    }

    geom_mean <- data.frame(data.table::transpose(data.frame(compositions::clo(c(compositions::geometricmean(data_left[!is.na(data_left)]),
                                                             compositions::geometricmean(data_top[!is.na(data_top)]),
                                                             compositions::geometricmean(data_right[!is.na(data_right)])), total = 100))))

    if (for_ternary) {
      return(c(round(geom_mean[[2]], digits = 0), round(geom_mean[[3]], digits = 0),round(geom_mean[[1]], digits = 0)))
    } else {
    left_mean <- round(geom_mean[[1]], digits = 0)
    top_mean <- round(geom_mean[[2]], digits = 0)
    right_mean <- round(geom_mean[[3]], digits = 0)
    return(list(left_mean = left_mean, top_mean = top_mean, right_mean = right_mean))
    }


  } else {
    if (mean_type == "arithmetic") {
    left_mean <- round(mean(data[!is.na(data[[triad_col_names[["left"]]]]), triad_col_names[["left"]]], na.rm = TRUE), digits = 0)
    top_mean <- round(mean(data[!is.na(data[[triad_col_names[["top"]]]]), triad_col_names[["top"]]], na.rm = TRUE), digits = 0)
    right_mean <- round(mean(data[!is.na(data[[triad_col_names[["right"]]]]), triad_col_names[["right"]]], na.rm = TRUE), digits = 0)
    }
  }

  return(list(left_mean = left_mean, top_mean = top_mean, right_mean = right_mean))
}

#' @title - Get the anchor titles to plot for a triad.
#' @param triad_id - The signifier id of the triad.
#' @param display_anchor_means - If TRUE, display the anchor means on the titles.
#' @param anchor_means - the anchor means to display - obtained from the calculate_triad_means function.
#' @param framework_object - the sensemakerframeworkr object associated with the triad.
#' @returns A named list of the triad anchor titles.
#' @export
get_triad_anchor_plot_titles <- function(triad_id, display_anchor_means, anchor_means, framework_object) {

  left_title <- clearBetweenHTMLTags(paste(stringr::str_replace_all(framework_object$get_triad_left_anchor_text(triad_id), "&amp;", "&"),  ifelse(display_anchor_means,  paste("\U003BC", "=", anchor_means[["left_mean"]]), "")), " ")
  right_title <-  clearBetweenHTMLTags(paste(stringr::str_replace_all(framework_object$get_triad_right_anchor_text(triad_id), "&amp;", "&"), ifelse(display_anchor_means, paste("\U003BC", "=", anchor_means[["right_mean"]]), "")), " ")
  top_title <- clearBetweenHTMLTags(paste(stringr::str_replace_all(framework_object$get_triad_top_anchor_text(triad_id), "&amp;", "&"),  ifelse(display_anchor_means, paste("\U003BC","=", anchor_means[["top_mean"]]), "")), " ")


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

#' @title Get a size value for plotting the triad anchor titles
#' @description
#' This might be a useful function - it is used by the workbench and plot_triad function and its return values are appropriate for Workbench sized plots.
#' @param anchor_titles - the named list of anchor plot titles obtained from the get_triad_anchor_plot_titles function.
#' @returns An integer for the anchor plot size.
#' @export
get_triad_anchor_plot_size <- function (anchor_titles) {

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

#' @title Get a size value for plotting the graph titles
#' @description
#' This might be a useful function - it is used by the workbench to size plot titles and useful for Workbench sized plot objects.
#' @param title - A string containing the title.
#' @param title_size_multiplier - A multiplier to expand or contract the retured size.
#' @returns An integer for the title plot size.
#' @export
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


#' @title Get the graph caption values
#' @description
#' Get a string containing the caption values from the passsed in dataset. This includes data count, N/A count, not responded count and so on.
#' @param filtered_data - the data being graphed
#' @param full_data - the full capture data set
#' @param sig_id - the signifier id being plotted.
#' @param framework_object, the sensemakerframeworkr object for the capture.
#' @returns A string containing the caption values.
#' @export
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

# Ternary Helpers
# These functions, which I hope will build up over time, will help with using the Ternary package for drawing ternary diagrams.
#' @title normalise ternary data to sum to 1
#' @description
#' Code to normalise data to sum to 1 - can also be done with compositions package but this is package neutral.
#' @param X - the data being normalised. 3 columns, top, right, left - the Ternary package standard.
#' @returns The input data with each row suming to 1 over the three columns.
#' @export
normalize_ternary <- function(X) {
  X <- as.matrix(X)
  X / rowSums(X)
}

#' @title ternary to <x,y>
#' @description
#' Take a 3 column ternary data and calculate the x,y values to return for standard plot.
#' @param X - the data being converted - 3 columns, top, right, left - the Ternary package standard.
#' @returns The data in two columns, x and y.
#' @export
ternary_to_xy <- function(X) {
  X <- sensemakerhelpersr::normalize_ternary(X)
  a <- X[, 1]
  b <- X[, 2]
  c <- X[, 3]

  x <- b + a / 2
  y <- a * sqrt(3) / 2

  cbind(x = x, y = y)
}

#' @title  <x,y> to ternary
#' @description
#' Take a 2 column ternary based x,y data set and calculate the three ternary values.
#' @param X - the data being converted - 2 columns, x and y values.
#' @returns The data in three columns, top, right, left - the Ternary package standard.
#' @export
xy_to_ternary <- function(xy) {
  x <- xy[, 1]
  y <- xy[, 2]

  a <- 2 * y / sqrt(3)
  b <- x - a / 2
  c <- 1 - a - b

  out <- cbind(A = a, B = b, C = c)

  # Numerical cleanup
  out[out < 0] <- 0
  out <- out / rowSums(out)

  out
}

#' @title  Calculate the confidence region around the meanfor a set of ternary data
#' @description
#' Supply a 3 column matrix and return the confidence interval around the mean
#' @param X - 3 column ternary data.
#' @param level - default 0.95, the confidence interval to calculate.
#' @param npoints - default 200, the number of points to use.
#' @param method - "hotelling" or "normal"
#' @returns The confidence interval data to graph in Ternary.
#' @export
mean_conf_region <- function(X, level = 0.95, npoints = 200,
                             method = c("hotelling", "normal")) {

   method <- match.arg(method)

  X <- sensemakerhelpersr::normalize_ternary(X)
  xy <- sensemakerhelpersr::ternary_to_xy(X)

  n <- nrow(xy)
  p <- 2  # 2D region in the ternary plane

  mu <- colMeans(xy)
  S <- stats::cov(xy)

  theta <- seq(0, 2 * pi, length.out = npoints)
  unit_circle <- cbind(cos(theta), sin(theta))

  if (method == "normal") {
    k <- stats::qchisq(level, df = p)
    Sigma_region <- S * k / n
  } else {
    # Hotelling-style region for the mean
    k <- p * (n - 1) / (n - p) * stats::qf(level, df1 = p, df2 = n - p)
    Sigma_region <- S * k / n
  }

  R <- chol(Sigma_region)
  ellipse_xy <- sweep(unit_circle %*% R, 2, mu, "+")

  sensemakerhelpersr::xy_to_ternary(ellipse_xy)
}


#' @title  Calculate the mean for the 3 columns of a ternary data matrix
#' @description
#' Supply a 3 column matrix and return the means of each column
#' @param X - 3 column ternary data.
#' @param mean_type - default "geometric, Use the geometric mean otherwise the arithmetic.
#' @param zero_logic - default "remove", values "small_value" or "remove". If "remove" all records with a zero value are removed, otherwise they are set to a value of 0.00001
#' @param total - default 1, values 1 or 100, which sets the 3 return values to sum to 1 or 100.
#' @returns The confidence interval data to graph in Ternary.
#' @export
group_mean_ternary <- function(X, mean_type = "geometric", zero_logic = "remove", total = 1) {
  X <- sensemakerhelpersr::normalize_ternary(X)
  if (mean_type == "geometric") {
    if (zero_logic == "small_value") {
      X[,1] <- ifelse(X[,1] <= 0, 0.00001, X[,1])
      X[,2] <- ifelse(X[,2] <= 0, 0.00001, X[,2])
      X[,3] <- ifelse(X[,3] <= 0, 0.00001, X[,3])
    } else {
      if (zero_logic == "remove") {
        X <- X[rowSums(X == 0) == 0, ]
      }
    }

    geom_mean <- compositions::clo(c(compositions::geometricmean(X[,1]), compositions::geometricmean(X[,2]), compositions::geometricmean(X[,3])), total = total)
    names(geom_mean) <- c("A", "B", "C")

  } else {

    geom_mean <- matrix(colMeans(X), nrow = 1,
           dimnames = list(NULL, colnames(X)))
  }
  return(geom_mean)
}


