


### Section for means tests.
#' @title Return the statistical output from a Hotelling T test.
#' @description
#' A list of the available colour brewer palettes available for heat maps.
#' @export
get_avalable_tests <- function() {
  return(c("hotelling", "dqdist.etest", "npmv_nonpartest", "adonis2"))
}

# Hotelling parametric and non-parametric test
#' @title Return the statistical output from a Hotelling T test.
#' @description
#' Perform a hotelling T^2 test on columns of data with a control variable.
#' @param data - The data frame with the data columns to test.
#' @param control_var - A vector of length nrows(data) with the test values.
#' @param non_parametric - Default TRUE, to perform a non-parametric T^2 test, otherwise FALSE to perform a parametric test.
#' @param b_value The number of permutations to perform on a non-parametric test.
#' @returns An object of class "hotelling.test" with the hotelling T^2 test result.
#' @export
perform_hotelling <- function(data, control_var, non_parametric = TRUE, b_value = 1000) {
  stopifnot(length(control_var == nrow(data)))
  stopifnot(is.logical(non_parametric))
  stopifnot(is.numeric(b_value))
  stopifnot(b_value > 0)
  perm_param <- ifelse(non_parametric,TRUE, FALSE)
  b_param <- ifelse(non_parametric, b_value, 0)
  hotelling <- Hotelling::hotelling.test(.~control_var, data = data, perm = perm_param, B = b_param,  progBar = FALSE)
  return(hotelling)
}


# Energy  non-parametric test
#' @title Return the statistical output from an Energy non_parametric test.
#' @description
#' Perform an Energy test on columns of data with a control variable. This is a non-parametric test.
#' @param data - The data frame with the data columns to test.
#' @param control_var - A vector of length nrows(data) with the test values.
#' @param non_parametric - Default TRUE, to perform a non-parametric  test. This value must always be TRUE.
#' @param b_value The number of permutations to perform on a non-parametric test.
#' @returns An object of class "htest" with the hotelling T^2 test result.
#' @export
perform_dqdist.etest <- function(data, control_var, non_parametric = TRUE, b_value = 1000) {
  stopifnot(length(control_var) == nrow(data))
  stopifnot(non_parametric == TRUE)
  stopifnot(is.numeric(b_value))
  stopifnot(b_value > 0)
  sizes <- as.numeric(table(control_var))
  result <- energy::eqdist.etest(x = data, sizes = sizes, R = b_value)
}

#
# Non-Parametric MANOVA (npmv package) test
#' @title Return the statistical output from an MANOVA test using the npmv package.
#' @description
#' Perform an MANOVA test on columns of data with a control variable. This is a non-parametric test.
#' @param data - The data frame with the data columns to test.
#' @param control_var - A vector of length nrows(data) with the test values.
#' @param non_parametric - Default TRUE, to perform a non-parametric  test. This value must always be TRUE.
#' @param b_value The number of permutations to perform on a non-parametric test.
#' @returns An object of class "list" (of 2 data frames) with the MANOVA test result. The first list entry contains the results, the second the relative effects for each response variable.
#' @export
perform_npmv_nonpartest <- function(data, control_var, non_parametric = TRUE, b_value = 1000) {
  stopifnot(length(control_var) == nrow(data))
  stopifnot(non_parametric == TRUE)
  stopifnot(is.numeric(b_value))
  stopifnot(b_value > 0)
  stopifnot(ncol(data) < 2)
  data1 <- as.data.frame(data)
  if (ncol(data) == 1) {
    colnames(data1) <- c("V1")
    result <- npmv::nonpartest(cbind(V1) ~ as.factor(control_var), data = data1, plots = FALSE, permreps = b_value)
  } else {
    colnames(data1) <- c("V1", "V2")
    result <- npmv::nonpartest(cbind(V1, V2) ~ as.factor(control_var), data = data1, plots = FALSE, permreps = b_value)
  }
return(result)
}

# Non-Parametric PERMANOVA (Permutation-based Multivariate Analysis of Variance) test
#' @title Return the statistical output from a Non-Parametric PERMANOVA (Permutation-based Multivariate Analysis of Variance) test.
#' @description
#' Non-Parametric PERMANOVA (Permutation-based Multivariate Analysis of Variance) test
#' @param data - The data frame with the data columns to test.
#' @param control_var - A vector of length nrows(data) with the test values.
#' @param non_parametric - Default TRUE, to perform a non-parametric  test. This value must always be TRUE.
#' @param b_value The number of permutations to perform on a non-parametric test.
#' @returns An object of class anova.cca"  "anova"      "data.frame" with the test result.
#' @export
perform_adonis2 <- function(data, control_var, non_parametric = TRUE, b_value = 1000) {
  stopifnot(length(control_var) == nrow(data))
  stopifnot(non_parametric == TRUE)
  stopifnot(is.numeric(b_value))
  stopifnot(b_value > 0)
  dist_matrix <- vegan::vegdist(as.data.frame(data), method = "euclidean")
  result <- vegan::adonis2(dist_matrix ~ as.factor(control_var), add = TRUE, permutations = b_value)
  return(result)
}
