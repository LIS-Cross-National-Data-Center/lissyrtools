#' Compute (weighted) counts or percentages from a list of data frames
#' 
#' @description
#' This function calculates (weighted) category counts or percentages for a given 
#' categorical variable across a list of data frames (e.g., by country or year). 
#' Optionally, results can be grouped by another categorical variable.
#'
#' @param data_list A named list of data frames, (e.g., across countries or years).
#' @param var_name A string specifying the name of the categorical variable for which counts or percentages are to be computed.
#'        This must be listed in `lissyrtools::lis_categorical_variables` or `lissyrtools::lws_wealth_categorical_variables`.
#' @param wgt_name (Optional) A string specifying the name of the weight variable to apply. If `NULL`, unweighted counts are used.
#' @param na.rm Logical; if `TRUE`, observations with missing values in `var_name` are removed before computing counts or percentages.
#' @param by (Optional) A string naming a second categorical variable for disaggregation. Results will then be split by this variable.
#'        Must also be listed in the allowed categorical variables.
#' @param percent Logical; if `TRUE`, the function returns weighted (or unweighted) percentages. 
#'        If `FALSE`, it returns simple category counts.
#'
#' @return A list of named vectors:
#'   - If `by` is not specified, returns a named vector of counts or percentages per dataset.
#'   - If `by` is specified, returns a nested list, where the outer list is by dataset and the inner list is by `by` category.
#' 
#' @details
#' - Any data frame where the `by` variable contains only `NA`s is dropped, with a warning.
#' 
#' @export
#' 
#' @examples
#' \dontrun{ 
#' data <- lissyrtools::lissyuse(data = c("de", "es", "uk"), vars = c("dhi", "region_c", "area_c", "educ", "emp"), from = 2016)
#' 
#' 
#' run_weighted_count(
#'  data[names(data)[stringr::str_sub(names(data),3,4) == "18"]], 
#'  var_name ="educ", 
#'  by = "emp", 
#'  percent = FALSE, 
#'  na.rm = TRUE
#' )
#' 
#' # Specify `percent` = TRUE, to output percentages, unweighted or weighted.
#' run_weighted_count(
#'  data[names(data)[stringr::str_sub(names(data),3,4) == "18"]], 
#'  var_name ="region_c", 
#'  percent = TRUE, 
#'  na.rm = FALSE
#' )
#' 
#' # It is also possible to check the share of missings. 
#' run_weighted_count(
#'  data[names(data)[stringr::str_sub(names(data),3,4) == "18"]], 
#'  var_name ="region_c", 
#'  percent = TRUE, 
#'  na.rm = TRUE
#' )  
#' 
#' 
#' # When `percent` = FALSE, and `wgt_name` is specified, it will be ignore and an unweighted count will be applied.
#' run_weighted_count(
#'  data[names(data)[stringr::str_sub(names(data),3,4) == "18"]], 
#'  var_name ="region_c", 
#'  wgt_name = "hpopwgt",
#'  percent = FALSE,
#'  na.rm = TRUE
#' ) 
#' 
#' #  Datasets where the variable in the `var_name` argument is only made of NA's will not be considered.
#' run_weighted_count(
#'  data[names(data)[stringr::str_sub(names(data),3,4) == "18"]], 
#'  var_name ="area_c", 
#'  percent = FALSE,
#'  na.rm = TRUE
#' ) 
#' 
#' # The same logic is applied with the `by` argument.
#' run_weighted_count(
#' data["uk15"], 
#' "educ", 
#' na.rm = TRUE, 
#' by = "area_c"
#' )
#' 
#'}
run_weighted_count <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  na.rm = FALSE,
  by = NULL,
  percent = FALSE
) {

  data_list <- lissyrtools::remove_dname_with_missings_in_weights(data_list, wgt_name) # return a list cleaned 
    
  # Check that var_name exists
    assertthat::assert_that(
      var_name %in% names(data_list[[1]]),
      msg = glue::glue(
        "Variable '{var_name}' could not be found as a column name in the datasets."
      )
    )

    # Check that all variables in `by` exist, if provided
    if (!is.null(by)) {
      assertthat::assert_that(
        by %in% names(data_list[[1]]),
        msg = glue::glue(
          "Grouping variable '{by}' could not be found as a column name in the datasets."
        )
      )
    }

    # Check that wgt_name exists, if provided
    if (!is.null(wgt_name)) {
      assertthat::assert_that(
        wgt_name %in% names(data_list[[1]]),
        msg = glue::glue(
          "Weight variable '{wgt_name}' could not be found as a column name in the datasets."
        )
      )
    }
    lissyrtools::check_input_in_weight_argument(wgt_name)

  allowed_categoricals_in_var_name <- c(lissyrtools::lis_categorical_variables, lissyrtools::lws_wealth_categorical_variables, "inum")
  if (!var_name %in% allowed_categoricals_in_var_name) {
    stop(sprintf("The `var_name` variable must be a categorical (not continuous) variable from `lissyrtools::lis_categorical_variables`, `lws_wealth_categorical_variables`, or the variable 'inum'."))
  }

  if (!is.null(by)) {
    allowed_categoricals_in_by <- c(lissyrtools::lis_categorical_variables, lissyrtools::lws_wealth_categorical_variables)
    if (!by %in% allowed_categoricals_in_by) {
      stop(sprintf("The `by` variable must be a categorical variable from `lissyrtools::lis_categorical_variables` or `lws_wealth_categorical_variables`."))
    }
  }

    df_to_keep <- purrr::map_lgl(data_list, ~{
      by_ok <- if (!is.null(by)) !all(is.na(.x[[by]])) else TRUE
      var_ok <- !all(is.na(.x[[var_name]])) 
      by_ok && var_ok
    })
    
    if (any(!df_to_keep)) {
      dropped <- names(data_list)[!df_to_keep]
      print(dropped)
      reasons <- purrr::map_chr(data_list[!df_to_keep], function(df) {
        by_na <- !is.null(by) && all(is.na(df[[by]]))
        var_na <- all(is.na(df[[var_name]]))
        paste0(
          if (by_na) paste0("`by` (", by, ") is all NA") else "",
          if (by_na && var_na) " and " else "",
          if (var_na) paste0("`var_name` (", var_name, ") is all NA") else ""
        )
      })
      warning(sprintf(
        "The following data frames were dropped due to missing data:\n%s",
        paste(paste0("- ", dropped, ": ", reasons), collapse = "\n")
      ))
      data_list <- data_list[df_to_keep]
    }
  

  if (percent == FALSE &&  !is.null(wgt_name)) {
    warning("`wgt_name` is ignored when `percent = FALSE`; a simple (unweighted) count is used instead.")
  }

  result <- purrr::imap(data_list, function(df, name) {
    if (!is.null(by)) {
      df[[by]] <- as.character(haven::as_factor(df[[by]]))
      df <- df[!is.na(df[[by]]), ]
      split_df <- split(df, df[[by]])

      result <- purrr::imap(split_df, function(sub_df, group_name) {
        var <- sub_df[[var_name]]
        wgt <- if (!is.null(wgt_name)) sub_df[[wgt_name]] else NULL
        lissyrtools::compute_weighted_count(
          var = var,
          wgt = wgt,
          percent = percent,
          na.rm = na.rm
        )
      })

      result  

    } else {
      var <- df[[var_name]]
      wgt <- if (!is.null(wgt_name)) df[[wgt_name]] else NULL
      lissyrtools::compute_weighted_count(
        var = var,
        wgt = wgt,
        percent = percent,
        na.rm = na.rm
      )
    }
  })

  return(result)

} 


#' Compute (Weighted) Counts or Percentages for a Categorical Variable 
#' 
#'
#'
#' @param var A column refering to one of the categorical variables in a LIS or LWS data frame.
#' @param wgt A numeric vector of weights (e.g., .x$hpopwgt, .x$pwgt). Must be the same length as \code{x}.
#' @param na.rm Logical; if \code{TRUE}, missing values in \code{x} and \code{w} are removed before computation. Default is \code{FALSE}.
#' @param percent Logical; if \code{TRUE}, computes weighted (or non-weighted) percentages.

#' @return A numeric vector, with category labels as names.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' data <- lissyrtools::lissyuse(data = "de20", vars = c("dhi", "age", "educ"))
#' compute_weighted_count(data$de20$educ, na.rm = TRUE)
#' compute_weighted_count(data$de20$educ, percent = TRUE)
#' compute_weighted_count(data$de20$educ, na.rm = TRUE, percent = TRUE)
#' compute_weighted_count(data$de20$educ, data$de20$ppopwgt, na.rm = TRUE, percent = TRUE)
#' compute_weighted_count(data$de20$educ, data$de20$ppopwgt, percent = TRUE)
#' }
compute_weighted_count <- function(
  var,
  wgt = NULL,
  na.rm = FALSE,
  percent = FALSE
) {


  if (is.null(wgt) || percent == FALSE) {
    wgt <- rep(1, length(var)) # this includes the NA's as well, so basically the number of rows of each data frame.
  }

  if (length(wgt) != length(var)) stop("Length of weights must match length of `var`.")



  var <- as.character(haven::as_factor(var)) # such that we can see the labels

  if (na.rm) {
    keep <- !is.na(var)
    var <- var[keep]
    wgt <- wgt[keep]
  }

  if (percent == FALSE) {
    t <- if (na.rm) table(var) else table(var, exclude = NULL)
    result <- as.vector(t)
    names(result) <- names(t)
  } else {
    # Unique categories (excluding NA if na.rm = TRUE)
    categories <- sort(unique(var[!is.na(var)]))
    result <- sapply(categories, function(cat) sum(wgt[var == cat], na.rm = TRUE))
    
    # Handle NA separately if na.rm = FALSE
    if (!na.rm && any(is.na(var))) {
      result_na <- sum(wgt[is.na(var)], na.rm = TRUE)
      result <- c(result, `NA` = result_na)
    }

    # Normalize to percentages
    result <- round(result / sum(wgt, na.rm = TRUE) * 100, 2)
  }

  return(result)
}



