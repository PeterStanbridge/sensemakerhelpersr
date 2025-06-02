


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
#' @title Do means tests
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
  stopifnot(all(c("from_id",	"to_id","from_title", "to_title") %in% colnames(means_tests)))

  if (is.character(filters)) {
    stopifnot(file.exists(filters))
    filters <- read.csv("filters", check.names = FALSE, stringsAsFactors = FALSE)
  }
  stopifnot(all(c("name", "title") %in% colnames(filters)))

  k <- 0

  result_list <<- vector("list", length = length(signifier_ids))
  names(result_list) <- signifier_ids
  purrr::walk(signifier_ids, function(sig_id) {

    sig_columns <-  fwd$sm_framework$get_anchor_compositional_column_names(sig_id, delist = TRUE)

    # This is the data frame we will be outputting the information we are interested in.
    stats_out <<- test_result_structure()

   # purrr::walk2(means_tests$from_id, means_tests$to_id, function(from_dat, to_dat) {

      purrr::pwalk(list(means_tests$from_id, means_tests$to_id, means_tests$from_title, means_tests$to_title), function(from_dat, to_dat, source1_title, source2_title) {

    #  source1_title <- as.character(unname(filters |> dplyr::filter(name == from_dat) |> dplyr::select(title)))
     # source2_title <- as.character(unname(filters |> dplyr::filter(name == to_dat) |> dplyr::select(title)))
    #  source1_title <- from_dat
    #  source2_title <- to_dat

      work_df <- dplyr::bind_rows(fwd$data[[from_dat]], fwd$data[[to_dat]])
      stopifnot("source" %in% colnames(work_df))
      cols_sel <- c(sig_columns, "source")
      sig_data <- work_df |> dplyr::select(dplyr::all_of(cols_sel)) |> stats::na.omit()
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


# do a correlation test on multi-select lists (mcqs)
#' @title do a correlation test on multi-select lists (mcqs)
#' @description
#' This function will perform a correlation test (Pearson's) for passed in list ids (or all if NULL) and a set list of correlation pair filters based on filtered cohort definitions.
#' @param correlation_pairs - The pairs of data filters to perform tests on. This can be either the file name or the data frame created from it or within the calling code. It must have columns at least "from_id", "to_id" where the ids are the names of the data frames held in the data list in the sensemakerdatar object.
#' @param fwd - The sensemakerdatar object for the framework being processed.
#' @param list_ids - Default NULL otherwise the signifier ids of the multi-select lists to be processed. If NULL then all the multiselect lists will be processed.
#' @param test_type - default Pearson", but the only type currently supported.
#' @returns Returns a list, the outer being one entry per correlation pair, the second being each multi-select list id, the third being each column in the dataset, and this list contains the test result list. This list has entries names "data", "expected", "residuals", "residuals_sqr", "p-value", "test_result", "used_p_simulation".
#' @export
calculate_multi_select_correlations <- function(correlation_pairs, fwd, list_ids = NULL, test_type = "Pearson") {

  if (1 == 3) {
    list_ids <- fwd$sm_framework$get_multiselect_list_ids()
  }

  stopifnot(is.character(test_type))
  stopifnot(test_type %in% c("Pearson"))

  if (!is.null(list_ids)) {
    stopifnot(is.vector(list_ids))
    stopifnot(all(list_ids %in% fwd$sm_framework$get_multiselect_list_ids()))
  } else {
    list_ids <- fwd$sm_framework$get_multiselect_list_ids()
  }

  if (is.character(correlation_pairs)) {
    stopifnot(stringi::stri_endswith(str = correlation_pairs, fixed =  ".csv"))
    stopifnot(file.exists(correlation_pairs))
    correlation_pairs <- read.csv(correlation_pairs, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    stopifnot(is.data.frame(correlation_pairs))
  }
  stopifnot(all(colnames(correlation_pairs %in% c("from_id", "to_id"))))

  # Get the multi-select list column names for each of the multi-select lists
  ms_list_columns <<- vector("list", length = length(list_ids))
  names(ms_list_columns) <- list_ids

  purrr::walk(names(ms_list_columns), ~ {ms_list_columns[[.x]] <<- fwd$sm_framework$get_list_column_mcq_names(.x, return_selected = TRUE)})

  # the final retured correlation pairs list that will be returned from this routine
  cp_results <<- vector("list", length = nrow(correlation_pairs))
  names(cp_results) <- paste0(correlation_pairs[, 1], "_", correlation_pairs[, 2])
  # Go through each pair of correlation test filters to perform the test.

  purrr::walk2(correlation_pairs[,1], correlation_pairs[,2], function(from_id, to_id) {

    if (1 == 3) {
      from_id <- correlation_pairs[1,1]
      to_id <- correlation_pairs[1,2]
    }

    # Now for each list

    # The muti-select list results returned as updated to the cp_reults list.
    ms_results <<- vector("list", length = length(list_ids))
    names(ms_results) <- list_ids

    purrr::walk(names(ms_list_columns), function(list_id) {
      if (1 == 3) {
        list_id <- names(ms_list_columns)[[1]]
      }
      col_names <- append(ms_list_columns[[list_id]], "source")
      all_data <-  dplyr::bind_rows(fwd$data[[from_id]], fwd$data[[to_id]]) |> dplyr::select(dplyr::all_of(col_names))

      col_results <<- vector("list", length = length(colnames(all_data)) - 1)
      names(col_results) <- colnames(all_data)[1:length(colnames(all_data)) - 1]

      purrr::walk(colnames(all_data)[1:length(colnames(all_data)) - 1], ~ {

        if (1 == 3) {
          x <- colnames(all_data)[[1]]
          tbl <- table(all_data[[x]], all_data$source)
        }

        # column returned results - a list with entries 1. the data table, 2, the residual table, 3, the residual square table, 4 the p-value, 5 pass-fail boolean
        test_results <<- vector("list", length = 7)
        names(test_results) <- c("data", "expected", "residuals", "residuals_sqr", "p-value", "test_result", "used_p_simulation")
        # we are going non-parametric if over 20% of the count values are less than 5
        tbl <- table(all_data[[.x]], all_data$source)
        tbl_values <- as.vector(tbl)
        len_values <- length(tbl_values)
        count_less_5 <- length(tbl_values[tbl_values < 5])
        per_less_5 <- round((count_less_5/len_values) * 100, digits = 0) >= 20


        if (test_type == "Pearson") {
          if (per_less_5) {
            chi_square_test <- chisq.test(tbl, simulate.p.value = TRUE, B = 5000, correct = FALSE)
          } else {
            chi_square_test <- chisq.test(tbl)
          }

          test_results[["data"]] <- chi_square_test$observed
          test_results[["expected"]] <- round(chi_square_test$expected, digits = 0)
          test_results[["residuals"]] <- round(chi_square_test$residuals, digits = 3)
          test_results[["residuals_sqr"]] <- round(chi_square_test$residuals^2, digits = 3)
          test_results[["p-value"]] <- round(chi_square_test$p.value, digits = 4)
          test_results[["test_result"]] <- chi_square_test$p.value < 0.05
          count_less_5 <- length(tbl_values[tbl_values < 5])
          per_less_5 <- round((count_less_5/len_values) * 100, digits = 0) >= 20
          test_results[["used_p_simulation"]] <- per_less_5
        } else {
          # fisher_test <- fisher.test(tbl)
        }

        col_results[[.x]] <<- test_results

      })

      ms_results[[list_id]] <<- col_results

    })

    cp_results[[paste0(from_id, "_", to_id)]] <<- ms_results

  })

  return(cp_results)

}


# get the correlation data by from and to signifier type
#' @title Get the correlation data for signifiers of a from and to signifier type
#' @description
#' This function will perform goodness of fit tests and return the results for each signifier combination of the from and to signifier types. For lists, these will be only the single select signifier mcqs and the date columns.
#' The actual tests are performed on the zone and region categorical columns for triads, dyads and stones.
#' @param df - The data to perform the tests - this is a data frame containing at least all the columns needed for the signifier types passed in. .
#' @param fw - The framework definition object.
#' @param from_type - Either "list" or one of the shape types ("dyad", "triad", "stones").
#' @param to_type - Either "list" or one of the shape types ("dyad", "triad", "stones").
#' @param round_digits - default 0, z, z^2, expected value table rounding digits.
#' @param residual_threshold - default 4, the threshold for residual calculation on residual squared values.
#' @param p_threshold - default 0.05, the p-value threshold for null hypothesis test.
#' @returns Returns a named list. ids_to_output contains the dataframe of signifier id pairs included in the output. sig_residuals a list of length the number of correlation pairs, It contains the "data", "expected", "residuals", "residuals_sqr", "p-value".
#' @export
get_correlations_by_type <- function(df, fw, from_type, to_type, from_signifier_classes = "signifier", to_signifier_classes = "signifier",
                                     from_signifier_ids = NULL,  to_signifier_ids = NULL, keep_only_include = TRUE, round_digits = 0, residual_threshold = 4, p_threshold = 0.05) {

  stopifnot(from_type %in% c("list", fw$get_shape_signifier_types()))
  stopifnot(to_type %in% c("list", fw$get_shape_signifier_types()))
  if (is.list(from_signifier_classes)) {
    from_signifier_classes <- unlist(purrr::keep(from_signifier_classes, ~ {!is.null(.x)}))
  }
  if (is.list(to_signifier_classes)) {
    to_signifier_classes <- unlist(purrr::keep(to_signifier_classes, ~ {!is.null(.x)}))
  }
  from_signifier_classes <- unlist(purrr::keep(from_signifier_classes, ~ {!is.na(.x)}))
  to_signifier_classes <- unlist(purrr::keep(to_signifier_classes, ~ {!is.na(.x)}))
  if (!is.null(from_signifier_classes)) {
    stopifnot(all(from_signifier_classes %in% fw$get_supported_signifier_classes()))
  }
  if (!is.null(from_signifier_classes)) {
    stopifnot(all(to_signifier_classes %in% fw$get_supported_signifier_classes()))
  }

  if (length(from_signifier_ids) > 0) {
    sig_types <- unique(unlist(purrr::map(from_signifier_ids, ~ {fw$get_signifier_type_by_id(.x)})))
    stopifnot(length(sig_type) > 1)
    stopifnot(sig_types != from_type)
    if (keep_only_include) {
    from_signifier_ids <- unlist(purrr::keep(from_signifier_ids, ~ {fw$get_signifier_include(.x)}))
    }
  }

  if (length(to_signifier_ids) > 0) {
    sig_types <- unique(unlist(purrr::map(to_signifier_ids, ~ {fw$get_signifier_type_by_id(.x)})))
    stopifnot(length(sig_type) > 1)
    stopifnot(sig_types != to_type)
    if (keep_only_include) {
      to_signifier_ids <- unlist(purrr::keep(to_signifier_ids, ~ {fw$get_signifier_include(.x)}))
    }
  }

  # The correlation columns will be coming from lists - e.g. dyad type will use the zone mcq equivalent columns for correlations.
  all_list_ids <- fw$get_list_ids(keep_only_include = keep_only_include)
  if (is.null(from_signifier_ids)) {
  from_ids  <- fw$get_signifier_ids_by_type(from_type, sig_class = from_signifier_classes, keep_only_include = keep_only_include)
  } else {
    from_ids <- from_signifier_ids
  }
  if (is.null(to_signifier_ids)) {
  to_ids  <- fw$get_signifier_ids_by_type(to_type, sig_class = from_signifier_classes, keep_only_include = keep_only_include)
  } else {
    to_ids <- to_signifier_ids
  }

  # If the from type is a shape type, then get the actual columns for the analysis
  done_dates <- FALSE
  if (from_type %in% fw$get_shape_signifier_types()) {
    # get those list signifier ids that have the same start string as the shape signifier ids
    from_col_ids <- unlist(purrr::imap(purrr::map(from_ids, ~ {stringr::str_starts(all_list_ids, .x)}), ~ {all_list_ids[purrr::map(from_ids, ~ {stringr::str_starts(all_list_ids, .x)})[[.y]]]}))
  } else {
    from_col_ids <- setdiff(fw$get_single_select_list_ids(sig_class = from_signifier_classes, keep_only_include = keep_only_include), "EntryYrMthDay")
    if ("date" %in% from_signifier_classes | "date" %in% to_signifier_classes) {
      dte_cols <- setdiff(fw$get_single_select_list_ids(sig_class = c("date"), keep_only_include = keep_only_include), "EntryYrMthDay")
      purrr::walk(dte_cols, ~ {df[[.x]] <<- as.character(df[[.x]])})
      done_dates <- TRUE
    }
  }

  if (to_type %in% fw$get_shape_signifier_types()) {
    # get those list signifier ids that have the same start string as the shape signifier ids
    to_col_ids <- unlist(purrr::imap(purrr::map(to_ids, ~ {stringr::str_starts(all_list_ids, .x)}), ~ {all_list_ids[purrr::map(to_ids, ~ {stringr::str_starts(all_list_ids, .x)})[[.y]]]}))
  } else {
    to_col_ids <- setdiff(fw$get_single_select_list_ids(sig_class = c("signifier", "date"), keep_only_include = keep_only_include), "EntryYrMthDay")
    if (!done_dates & ("date" %in% from_signifier_classes | "date" %in% to_signifier_classes)) {
      dte_cols <- setdiff(fw$get_single_select_list_ids(sig_class = c("date"), keep_only_include = keep_only_include), "EntryYrMthDay")
      purrr::walk(dte_cols, ~ {df[[.x]] <<- as.character(df[[.x]])})
    }
  }

  if (from_type != to_type) {
    ids_to_output <- data.frame(tidyr::crossing(from_col_ids, to_col_ids))
  } else {
    ids_to_output <- data.frame(t(utils::combn(from_col_ids, m = 2)))
  }

  colnames(ids_to_output) <- c("from", "to")

  sig_residuals <- purrr::map2(ids_to_output[["from"]], ids_to_output[["to"]], ~ {get_residuals(df, fw, .x, .y, round_digits, residual_threshold, p_threshold)})
  names(sig_residuals) <- paste0(ids_to_output[["from"]], "_", ids_to_output[["to"]])
  from_ids <- unlist(purrr::map(ids_to_output[["from"]], ~ {stringr::str_split_i(.x, pattern = "_", i = 1)}))
   to_ids <- unlist(purrr::map(ids_to_output[["to"]], ~ {stringr::str_split_i(.x, pattern = "_", i = 1)}))

   ids_out <- data.frame(from = from_ids, to = to_ids)
    return(list(sig_residuals = sig_residuals, ids_to_output = ids_out))

}


# Calculate the goodness of test residual for a from and to column for a from and to types.
#' @title Get the correlation data for signifiers of a from and to signifier type
#' @description
#' This function will perform goodness of fit tests and return the results for each signifier combination of the from and to signifier types. For lists, these will be only the single select signifier mcqs and the date columns.
#' The actual tests are performed on the zone and region categorical columns for triads, dyads and stones.
#' @param df - The data to perform the tests - this is a data frame containing at least all the columns needed for the signifier types passed in. .
#' @param fw - The framework definition object.
#' @param from_col - The from data column name in data frame df for correlation calculation
#' @param to_col - The to data column name in data frame df for correlation calculation. Must not be the same as from_col
#' @param round_digits - default 0, z, z^2, expected value table rounding digits.
#' @param residual_threshold - default 4, the threshold for residual calculation on residual squared values.
#' @param p_threshold - default 0.05, the p-value threshold for null hypothesis test.
#' @returns Returns a named list.containing the residual calculations. z, zsqr  the data count matrix, expected value matrix, p_value, test_result accept null hypothesis TRUE or FALSE, .
#' @export
get_residuals <- function(df, fw, from_col, to_col, round_digits = 0, residual_threshold = 4, p_threshold = 0.05) {

  if (!(from_col %in% fw$get_all_signifier_ids())) {
    from_id <- stringr::str_split_i(from_col, pattern = "_", i = 1)
  } else {
    from_id <- from_col
  }
  if (!(to_col %in% fw$get_all_signifier_ids())) {
    to_id <- stringr::str_split_i(to_col, pattern = "_", i = 1)
  } else {
    to_id <- to_col
  }

  from_type <- fw$get_signifier_type_by_id(from_id)
  to_type <- fw$get_signifier_type_by_id(to_id)

  filterString <- paste0("!is.na(", paste0("`", from_col, "`"), ") & !is.na(", paste0("`", to_col, "`"), ") & ",
                         paste0("`", from_col, "`"), " != 'NA' & ", paste0("`", to_col, "`"), " != 'NA'", " & ",
                         paste0("`", from_col, "`"), "!=   '' & ", paste0("`", to_col, "`"), " !=   ''")
  filterStringExpression <- parse(text = filterString)
  # we have to have more than two options left on removing the NAs etc in order to do the test properly - i.e get a proper table of residuals.
  t1a <- df |> dplyr::filter(eval(filterStringExpression))  |> dplyr::select(dplyr::all_of(c(from_col, to_col)))

  if (nrow(t1a) == 0) {return(list(z = NULL, zsqr = NULL, data = NULL, p_value = NULL))}
  if (length(unique(t1a[[1]])) < 2 | length(unique(t1a[[2]])) < 2) {return(list(z = NULL, zsqr = NULL, data = NULL, p_value = NULL))}

  colnames(t1a)[[1]] <- fw$get_signifier_title(from_id)
  colnames(t1a)[[2]] <- fw$get_signifier_title(to_id)

  # For list pull out titles to replace keys
  if (from_type == "list" && fw$get_list_max_responses(from_id) == 1) {
    t1a[, 1] <- unlist(unname(purrr::map(t1a[, 1], ~ {fw$get_list_item_title(from_id, .x)})))
  }
  if (to_type == "list" && fw$get_list_max_responses(to_id) == 1) {
    t1a[, 2] <- unlist(unname(purrr::map(t1a[, 2], ~ {fw$get_list_item_title(to_id, .x)})))
  }

  t1 <- as.array(table(t1a))
  df2 <- as.data.frame(plyr::adply(t1, c(1,2)))
  df2[["V1"]] <- as.numeric(df2[["V1"]])

  udfStats <-matrix(unlist(df2),dim(df2));
  dataTabMatrix <- xtabs(as.numeric(udfStats[,3]) ~ udfStats[,1]+udfStats[,2],df2)

  chiTest <- chisq.test(dataTabMatrix, simulate.p.value = TRUE)

  dta <- chiTest$observed
  colnames(dta) <- colnames(t1)
  rownames(dta) <- rownames(t1)
  expected <- round(chiTest$expected, digits = round_digits)
  colnames(expected) <- colnames(t1)
  rownames(expected) <- rownames(t1)
  z1 <- chiTest$stdres
  z <- round(z1, digits = round_digits)
  colnames(z) <- colnames(t1)
  rownames(z) <- rownames(t1)
  zsqr1 <- z1^2
  zsqr <- round(zsqr1, digits = round_digits)
  colnames(zsqr) <- colnames(t1)
  rownames(zsqr) <- rownames(t1)
  p_value <- round(chiTest$p.value, digits = 4)
  test_result <- chiTest$p.value < p_threshold

  test_results_sqr <- data.frame(row_val = character(0), col_val = character(0), residual_sqr = numeric(), type = character(0))


  for (i in seq_along(rownames(zsqr))) {

    for (j in seq_along(colnames(zsqr))) {
      # zero columns or rows can come out as NaN so if this is so, make zero
      if (is.nan(zsqr[i, j])) {
        zsqr[i, j] <- 0
      }
    }
  }

  for (i in seq_along(rownames(zsqr))) {

    for (j in seq_along(colnames(zsqr))) {

      if (zsqr1[i, j] >= residual_threshold) {
        temp_df_sqr <- data.frame(row_val = rownames(zsqr)[[i]], col_val = colnames(zsqr)[[j]], residual_sqr = zsqr1[i, j], type = ifelse(z1[i, j] < 0, "Negative", "Positive"))
        test_results_sqr <- dplyr::bind_rows(test_results_sqr, temp_df_sqr)
      }
    }

  }

  return(list(z = z, zsqr = zsqr, data = dta, expected = expected, p_value = p_value, test_result = test_result, test_result_detail = test_results_sqr, z1 = z1, zsqr1 = zsqr1))
}


# Build a corpus from a list of character data.
#' @title Build a corpus from a list of character data
#' @description
#' This function uses the library quanteda to create a token list (stemmed and unstemmed both with stop words removed), the document term matrix and the trimmed document term matrix.
#' The data is returned as a list of lists.
#' @param df - The data frame to create the corpus from - must include columns named in freetext_id and doc_var parameters.
#' @param framework_data - The framework data object.
#' @param freetext_id - The freetext column name containing the text to create the corpus from
#' @param doc_var - The column name to use as the document variable.
#' @param min_term_freq - Default 3, number of occurrence of a term before it it is accepted into the corpus.
#' @param languages - Default "en", a vector of supported 2 character language codes for use in stop words and stemming.
#' @returns Returns a named list of tokens_stem (the stemmed tokens with stop words removed), tokens_unstem (the unstemmed tokens with stop words removed), dtm (the document term matrix not trimmed) and dtm_trim, the document term matrix trimmed to the min_term_freq value.
#' @export
build_corpus <- function(df, framework_data,  freetext_id, doc_var, min_term_freq = 3, languages = "en") {

  stopifnot(doc_var %in% colnames(df))
  stopifnot(freetext_id %in% colnames(df))
  stopifnot(is.numeric(min_term_freq))
  stopifnot(length(min_term_freq) == 1)
  stopifnot(min_term_freq > 2)

  if (!(all(languages == "en") & !exists("isoLanguages"))) {
    get_iso_codes()
    stopifnot(all(languages %in% isoLanguages[["Code"]]))
  }


  fragment_text_corpus <- quanteda::corpus(df[[freetext_id]], docvars = data.frame(doc_var = df[[doc_var]]))

  tokens <- quanteda::tokens(fragment_text_corpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE,
                             remove_url = TRUE, remove_separators = TRUE, split_hyphens = TRUE, split_tags = TRUE)

  purrr::walk(languages, ~ {fragment_token_stemmed <<- quanteda::tokens_wordstem(tokens, language = .x)})

  purrr::walk(languages, ~ {fragment_token_stemmed <<- quanteda::tokens_remove(fragment_token_stemmed, quanteda::stopwords(.x))})

  fragment_token_stemmed <- quanteda::tokens_remove(fragment_token_stemmed, framework_data$stop_words)

  dtm.stemmed <- quanteda::dfm(fragment_token_stemmed, tolower = TRUE)
  dtm.trim_stemmed <- quanteda::dfm_trim(dtm.stemmed, min_termfreq = 3)


  purrr::walk(languages, ~ {fragment_token_unstemmed <<- quanteda::tokens_remove(tokens, quanteda::stopwords(.x))})

  fragment_token_unstemmed <- quanteda::tokens_remove(fragment_token_unstemmed, framework_data$stop_words)

  dtm.unstemmed <- quanteda::dfm(fragment_token_unstemmed, tolower = TRUE)
  dtm.trim_unstemmed <- quanteda::dfm_trim(dtm.unstemmed, min_termfreq = 3)

  return(list(text_corpus = fragment_text_corpus, tokens_stem = fragment_token_stemmed, tokens_unstem = fragment_token_unstemmed, dtm_stemmed = dtm.stemmed, dtm_trim_stemmed = dtm.trim_stemmed, dtm_unstemmed = dtm.unstemmed, dtm_trim_unstemmed = dtm.trim_unstemmed))
}

# Build a pairs data frame - one data frame binding rows from two data frames. .
#' @title Build a set of pairs data frames - one data frame binding rows from two data frames.
#' @description
#' This function takes a dataframe of pairs definitions from existing data frames and creates new paired data frames.
#' @param framework_data - The framework data object.
#' @param pairs_definitions - A data frame that must have at least two columns, one with name "from_id" the other "to_id" which are both data frame names in framework_data$data
#' @param doc_var - Default "none", indicating the document variable name to give each dataset being paired. This will add a new column to the resultant data frame whose values indicate which data frame it came from. Used in many stats functions like keyness indicators. "none" means leave the data frame as per the originals. "auto" means use the from and to ids as the doc variable. Otherwise a data frame with from_var and to_var column names with the same number of rows as the pairs_definitions data frame.
#' @param plot_col - Default "none", indicating the document variable colours to use in any pair graphing. This will add a new column to the resultant data frame whose values indicate which colour to use. Used in many stats functions like keyness indicators. "none" means leave the data frame as per the originals. "auto" means use the from and to colours as the doc colours Otherwise a data frame with from_col and to_col column names with the same number of rows as the pairs_definitions data frame.
#' @returns Returns a named list of tokens_stem (the stemmed tokens with stop words removed), tokens_unstem (the unstemmed tokens with stop words removed), dtm (the document term matrix not trimmed) and dtm_trim, the document term matrix trimmed to the min_term_freq value.
#' @export
build_pair_datasets <- function(framework_data, pairs_definitions, doc_var = "none", plot_col = "none") {

  if (is.data.frame(doc_var)) {
    stopifnot(all(c("from_var", "to_var") %in% colnames(doc_var)))
    stopifnot(all(doc_var[["from_var"]] %in% framework_data$get_data_list_names(export_only = TRUE)))
    stopifnot(all(doc_var[["to_var"]] %in% framework_data$get_data_list_names(export_only = TRUE)))
  } else {
    stopifnot(is.character(doc_var))
    stopifnot(length(doc_var) == 1)
    stopifnot(doc_var %in% c("none", "auto"))
    if (doc_var == "auto") {
      stopifnot(all(c("from_id", "to_id") %in% colnames(pairs_definitions)))
      stopifnot(all(pairs_definitions[["from_id"]] %in% framework_data$get_data_list_names(export_only = TRUE)))
      stopifnot(all(pairs_definitions[["to_id"]] %in% framework_data$get_data_list_names(export_only = TRUE)))
      doc_var <- data.frame(from_var = pairs_definitions[["from_id"]], to_var =  pairs_definitions[["to_id"]])
    }
  }

  if (is.data.frame(plot_col)) {
    stopifnot(all(c("from_col", "to_col") %in% colnames(plot_col)))
    stopifnot(all(areColors(plot_col[["from_col"]])))
    stopifnot(all(areColors(plot_col[["to_col"]])))

  } else {
    stopifnot(is.character(plot_col))
    stopifnot(length(plot_col) == 1)
    stopifnot(plot_col %in% c("none", "auto"))
    if (plot_col == "auto") {
      stopifnot(all(c("from_colour", "to_colour") %in% colnames(pairs_definitions)))
      stopifnot(all(areColors(pairs_definitions[["from_colour"]])))
      stopifnot(all(areColors(pairs_definitions[["to_colour"]])))
      plot_col <- data.frame(from_col = pairs_definitions[["from_colour"]], to_col =  pairs_definitions[["to_colour"]])
    }
  }


  from_ids <- pairs_definitions[, "from_id"]
  to_ids <- pairs_definitions[, "to_id"]
  from_colours <- pairs_definitions[, "from_colour"]
  to_colours <- pairs_definitions[, "to_colour"]
  filters_used <- unique(append(from_ids, to_ids))
  purrr::pwalk(list(from_ids, to_ids, from_colours, to_colours), function(from_id, to_id, from_colour, to_colour) {
    #tmp_from and with the quanteda keyness processing doc_var added then row-bind into a single data frame and add to the data list. Add colours to it too.
    tmp_from <- framework_data$data[[from_id]]
    tmp_from[["doc_var"]] <- rep_len(x = from_id, length.out = nrow(tmp_from))
    tmp_from[["plot_col"]] <- rep_len(x = from_colour, length.out = nrow(tmp_from))
    tmp_to <- framework_data$data[[to_id]]
    tmp_to[["doc_var"]] <- rep_len(x = to_id, length.out = nrow(tmp_to))
    tmp_to[["plot_col"]] <- rep_len(x = to_colour, length.out = nrow(tmp_to))
    framework_data$add_data_data_frame(dplyr::bind_rows(tmp_from, tmp_to), name = paste0(from_id, "_", to_id), add_to_export_list_names = TRUE)
  })

}

# Which rows of a data frame etc are all zero
#' @title Which rows of a numeric data frame are all zeros
#' @description
#' This function takes in an object that can be turned into a numeric matrix and returns for each row, whether they are all zero.
#' @param dta - The data that can be turned into a numeric matrix, such as a data frame or term frequency matrix
#' @returns Returns a vector of truth values, TRUE if the whole row is zero otherwise FALSE.
#' @export
are_rows_zero <- function(dta) {
  row_test <- unlist(purrr::imap(unname(as.matrix(dta))[,1], ~ {all((unname(as.matrix(dta))[1:nrow(unname(as.matrix(dta))), ] == 0)[.y,])}))
  return(row_test)
}

# Which rows of a data frame etc are all zero
#' @title Are there sufficient rows to produce a keyness graph
#' @description
#' This function takes in an object that can be turned into a numeric matrix and returns TRUE or FALSE as to whether there is sufficient rows in a term document matrix to produce a keyness plot.
#' @param dta - The data that can be turned into a numeric matrix, such as a data frame or term frequency matrix
#' @returns TRUE or FALSE, the term document matrix can be printed
enough_rows <- function(dta) {
  row_test <- are_rows_zero(dta)
  true_row_count <- length(which(row_test == TRUE))
  false_row_count <- length(which(row_test == FALSE))
  return(false_row_count > true_row_count + 2)
}
