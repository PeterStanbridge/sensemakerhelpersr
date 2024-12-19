


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
  stopifnot(length(control_var) == nrow(data))
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

# do means tests
# Do means tests on data passed into the function.
#' @description
#' Do means tests on the data passed into the function.
#' @param filters - filters from a filter file. Can be either the file name or the data frame created from it. It have columns (at least) of "name" and "title". Note that the actual filter file will also contain an expression column, an include column and colour column. The function is already expecting the filter to have been executed against the {sensemakerdatar} object. It is used here to pick up titles.
#' @param means_tests - means tests from a means test file. Can be either the file name or the data frame created from it. It must have columns at least "from_id", "to_id" and "name". The from_id and to_id are names of data filters in the filter parameter.
#' @param fwd - The sensemakerdatar object for the framework being processed.
#' @param signifier_ids - Default NULL otherwise the signifier ids of the signifiers to be processed. These should be either triad and/or dyad signifier ids. If NULL then the signifier types parameter must be entered.
#' @param signifier_types - Default NULL otherwise the signifier types to be processed. Values should be either "triad", "dyad" or both. If NULL then the signifier ids parameter must be entered.
#' @param test_type - default "hotelling". Must be one of the supported test types, which can be found by calling sensemakerhelpersr::get_avalable_tests()
#' @param non_parametric - default TRUE, otherwise FALSE. Only applicable (for now) in the hotelling test, the other supported tests are all non-parametric.
#' @param b_value - the bootstrap iteration value for many of the non-parametric tests. Default 1000.
#' @returns Returns a list, one for each signifier id, of the test results in the standard CE SenseMaker Workbench format.
#' @export
do_means_tests <- function(filters, means_tests, fwd, signifier_ids = NULL, signifier_types = NULL, test_type = "hotelling", non_parametric = TRUE, b_value = 1000) {

  stopifnot((is.null(signifier_ids) & !is.null(signifier_types) | (!is.null(signifier_ids) & is.null(signifier_types))))
  stopifnot(class(means_tests) %in% c("character", "data.frame"))
  stopifnot(class(filters) %in% c("character", "data.frame"))
  stopifnot(test_type %in% get_avalable_tests())
  # signifier types must be shape signifier types
  if (!is.null(signifier_types)) {
    stopifnot(all(signifier_types %in% fwd$sm_framework$get_shape_signifier_types()))
    signifier_ids <- unlist(unname(purrr::map(signifier_types, ~ {fwd$sm_framework$get_signifier_ids_by_type(.x)})))
  }

  if (is.character(means_tests)) {
    stopifnot(file.exists(means_tests))
    means_tests <- read.csv("means_tests.csv", check.names = FALSE, stringsAsFactors = FALSE)
  }
  stopifnot(all(c("from_id",	"to_id","name") %in% colnames(means_tests)))

  if (is.character(filters)) {
    stopifnot(file.exists(filters))
    filters <- read.csv("filters", check.names = FALSE, stringsAsFactors = FALSE)
  }
  stopifnot(all(c("name", "title") %in% colnames(filters)))

  k <- 0

  result_list <- vector("list", length = length(signifier_ids))
  names(result_list) <- signifier_ids
  purrr::walk(signifier_ids, function(sig_id) {

    sig_columns <-  fwd$sm_framework$get_anchor_compositional_column_names(sig_id, delist = TRUE)

    # This is the data frame we will be outputting the information we are interested in.
    stats_out <- test_result_structure()

    purrr::walk2(means_tests$from_id, means_tests$to_id, function(from_dat, to_dat) {

      source1_title <- as.character(unname(filters |> dplyr::filter(name == from_dat) |> dplyr::select(title)))
      source2_title <- as.character(unname(filters |> dplyr::filter(name == to_dat) |> dplyr::select(title)))

      work_df <- dplyr::bind_rows(fwd$data[[from_dat]], fwd$data[[to_dat]])
      stopifnot("source" %in% colnames(work_df))
      cols_sel <- c(sig_columns, "source")
      sig_data <- work_df |> dplyr::select(all_of(cols_sel)) |> na.omit()
      col_contents <- get_sig_stats_names(sig_id, fwd)
      colnames(sig_data) <- col_contents$col_names
      filter_string <- paste0("sig_data %>% dplyr::filter(", col_contents$query_string, ")")
      filter_query <- parse(text = filter_string)
      sig_data <- eval(filter_query)
      col_number <- ncol(sig_data) * -1

      total_transformed <- compositions::ilr(sig_data[,col_number])

      test_results <- perform_required_test(total_transformed, sig_data$source, non_parametric, b_value, test_type, source1_title, source2_title)

      stats_out <<- dplyr::bind_rows(stats_out, test_results)




    })

    stats_out[["statistic"]] <- round(stats_out[["statistic"]], digits = 4)
    stats_out[["p_Value"]] <- round(stats_out[["p_Value"]], digits = 4)
    stats_out[["m"]] <- round(stats_out[["m"]], digits = 4)
    result_list[[sig_id]] <<- stats_out

  })
  return(result_list)

}

get_sig_stats_names <- function(id, fwd) {

  sig_type <- fwd$sm_framework$get_signifier_type(id = id)
  if (sig_type == "triad") {
    col_names <- c("x", "y", "z", "source")
    query_string <- "x !=0 & y != 0 & z != 0"
    return(list(col_names = col_names, query_string = query_string))
  }
  if (sig_type == "dyad") {
    col_names <- c("x", "y", "source")
    query_string <- "x !=0 & y != 0"
    return(list(col_names = col_names, query_string = query_string))
  }

}

perform_required_test <- function(data, control_var, non_parametric, b_value, test_type, source1_title, source2_title) {
  call_function <- paste0("perform_", test_type)
  test_result <- do.call(call_function, args = list(data = data, control_var = control_var, non_parametric = non_parametric, b_value = b_value))
  call_function <- paste0("format_return_", test_type)
  formatted_test_result <- do.call(call_function, args = list(test_result = test_result, source1_title = source1_title, source2_title = source2_title))
  return(formatted_test_result)
}

test_result_structure <- function() {
  stats_out <- data.frame(group1 = character(), group2 = character(), statistic = double(), m = double(), df1 = integer(), df2 = integer(),
                          nx = integer(), ny = integer(), p = integer(), p_Value = integer(), Accept_Null = logical())
  return(stats_out)
}

format_return_hotelling <- function(test_result, source1_title, source2_title) {
  stats <- list(group1 = source1_title, group2 = source2_title, statistic = test_result$stat$statistic, m = test_result$stat$m, df1 = test_result$stat$df[[1]],
                df2 = test_result$stat$df[[2]], nx = test_result$stat$nx, ny = test_result$stat$ny, p = test_result$stat$p, p_Value = test_result$pval,
                Accept_Null = test_result$pval > 0.05)
  return(stats)
}
