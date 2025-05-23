

#' Apply IQR-Based Top and Bottom Coding to LIS/LWS Variables
#'
#' @description
#' This function performs top and/or bottom coding on a specified variable across a list of LIS/LWS datasets.
#' It applies an interquartile range (IQR)-based rule on the \code{log()} transformation of the variable.
#' Optionally, weights can be supplied, and the transformation can be one- or two-sided.
#' 
#' 
#' @param data_list A named list of data frames, from LIS or LWS microdata.
#' @param var_name Character string. Name of the variable to code (e.g., "dhi").
#' @param wgt_name Optional character string. Name of the weight variable to use in computing weighted percentiles.
#' @param times Numeric. The IQR multiplier for determining bounds (default is 3).
#' @param one_sided Character. Set to \code{"top"}, \code{"bottom"}, or \code{NULL} for two-sided coding.
#' @param type Character. Type of quantile estimator to use (default is \code{"type_4"}).
#' 
#' 
#' @return A list of data frames with the same structure as \code{data_list}, where \code{var_name}
#' has been adjusted by bounding extreme values according to an IQR rule.
#' 
#' @details
#' The function:
#' \itemize{
#'   \item Transforms the variable to \code{log()} scale (logarithmic transformation).
#'   \item Replaces invalid log-values (e.g. from log(0) or negatives) with zero.
#'   \item Computes the IQR (interquartile range) on the log-transformed variable using weighted percentiles.
#'   \item Caps values beyond \eqn{[Q1 - times * IQR, Q3 + times * IQR]} on the original scale using \code{exp()}.
#' }
#'
#' Regarding LWS datasets:
#' \itemize{
#'   \item Datasets with multiple imputations (via \code{inum}) are detected automatically.
#'   \item Top and bottom coding is applied **within each imputation group** in such datasets.
#'   \item Datasets without \code{inum}, or with only a single imputation, are processed normally.
#' } 
#' 
#' A warning is issued if the variable level (e.g. household vs individual) seems inconsistent with the dataset structure.
#'
#' @export
#' 
#' @examples
#' \dontrun{ 
#' # Import data, ideally at the level of the variable of interest.
#' data_hhd <- lissyrtools::lissyuse("au", vars = c("dhi"), from = 2016)
#' 
#' # Default case, where top and bottom coding is performed simultaneously
#' data_hhd[1]  %>%
#'  purrr::map(~ .x[!is.na(.x$dhi), ]) %>%
#'  purrr::map(~ .x %>% mutate(new_wgt = nhhmem * hwgt)) %>%
#'  apply_iqr_top_bottom_coding("dhi", "hwgt", times = 3) %>% 
#'  run_weighted_mean("dhi", "new_wgt")
#' 
#' # Example with the use or arguments `one_sided` = {"top", "bottom"} and `type`
#' data_hhd[1]  %>%
#'  purrr::map(~ .x[!is.na(.x$dhi), ]) %>%
#'  purrr::map(~ .x %>% mutate(new_wgt = nhhmem * hwgt)) %>%
#'  apply_iqr_top_bottom_coding("dhi", "hwgt", one_sided = "top", type = "type_2") %>% 
#'  run_weighted_mean("dhi", "new_wgt")
#' 
#' # Load individual-level datasets by selecting individual-level variables, if the target variable is at the individual level (e.g., "pilabour")
#' data_ind <- lissyrtools::lissyuse("au", vars = c("pilabour", "emp"), from = 2016)
#' 
#' data_ind[1]  %>%
#'  purrr::map(~ .x[!is.na(.x$pilabour), ] %>% filter(emp ==1)) %>%
#'  apply_iqr_top_bottom_coding("pilabour", "ppopwgt") %>% 
#'  run_weighted_percentiles("pilabour", "ppopwgt", probs = seq(0.1, 0.9,0.1))
#' }
apply_iqr_top_bottom_coding <- function(data_list, var_name, wgt_name = NULL, times = 3, one_sided = NULL, type = c("type_4", "type_2")) {

    type <- match.arg(type)
    
    # --- Clean data --- 
    data_list <- lissyrtools::remove_dname_with_missings_in_weights(data_list, wgt_name) # return a list cleaned 
    lissyrtools::check_input_in_weight_argument(wgt_name) 

  
    # Check var_name exists
    assertthat::assert_that(
      var_name %in% names(data_list[[1]]),
      msg = glue::glue(
        "Variable '{var_name}' could not be found as a column name in the datasets."
      )
    )

    # Check wgt_name exists
    if (!is.null(wgt_name)) {
      assertthat::assert_that(
        wgt_name %in% names(data_list[[1]]),
        msg = glue::glue(
          "Weight variable '{wgt_name}' could not be found as a column name in the datasets."
        )
      )
    }

    # --- inum stage --- 
 
    # Check if inum exists
    inum_present <- "inum" %in% names(data_list[[1]])

    # Identify which datasets have multiple imputations
    datasets_with_multiple_inum <- NULL
    if (inum_present) {
     datasets_with_multiple_inum <- names(data_list)[
      purrr::map_lgl(data_list, ~ length(unique(.x$inum)) > 1)
     ]
    }
  
    # --- Warning on level mismatches between variable and datasets ---
    if (
      ((var_name %in%
        c(
          lissyrtools::lis_household_variables,
          lissyrtools::lws_household_variables
        )) &&
        ("pid" %in% names(data_list[[1]])))
    ) {
      warning(glue::glue( # once a individual level variable is loaded in lissyuse, dataset automatically becomes p-level too. 
        "Level mismatch: the variable '{var_name}' is household-level, ",
        "but the dataset appears to be at the individual level." 
      ))
    } else if (!(var_name %in% c(lissyrtools::lis_variables, lissyrtools::lws_variables))) {
      warning(glue::glue(
        "Variable '{var_name}' not recognized: please ensure it matches the level ",
        "(household or individual) of the datasets being used."
      ))
    }


    # Body 
    result <- purrr::imap(data_list, function(df, name) {
      var <- df[[var_name]]
      wgt <- if (!is.null(wgt_name)) df[[wgt_name]] else NULL
    
      if (!is.null(datasets_with_multiple_inum) && name %in% datasets_with_multiple_inum) {
        # Process separately for each imputation
        processed_df <- df %>%
          dplyr::group_split(inum) %>%
          purrr::map_dfr(function(group_df) {
            coded_var <- iqr_top_bottom_coding(
              x = group_df[[var_name]],
              w = if (!is.null(wgt_name)) group_df[[wgt_name]] else NULL,
              times = times,
              type = type,
              one_sided = one_sided
            )
            group_df[[var_name]] <- coded_var
            group_df
          })
        return(processed_df)
      } else {
        # Process normally
        coded_var <- iqr_top_bottom_coding(
          x = var,
          w = wgt,
          times = times,
          type = type,
          one_sided = one_sided
        )
        df[[var_name]] <- coded_var
        return(df)
      }
    })
  
    return(result)
}

#' IQR-Based Top/Bottom Coding for a Single Variable
#' 
#' @description
#' Internal utility that applies top and/or bottom coding to a numeric vector using an IQR rule
#' on the \code{log()} transformation.
#' 
#' @param x Numeric vector. Variable to be top/bottom coded.
#' @param w Optional numeric vector of weights.
#' @param times Numeric. Multiplier for IQR to determine bounds (default is 3).
#' @param type Character. Quantile estimation type, passed to `compute_weighted_percentiles` function (e.g., \code{"type_4"}, \code{"type_2"}).
#' @param one_sided Character. \code{"top"}, \code{"bottom"}, or \code{NULL} (default) for two-sided coding.
#' 
#' @return A numeric vector with values above or below the threshold replaced (on the original scale). 
#' @export
#' 
#' @examples
#' \dontrun{
#' data <- lissyrtools::lissyuse(data = "it16", vars = c("dhi"))
#' iqr_top_bottom_coding(data$it16h$dhi)
#' iqr_top_bottom_coding(data$it16h$dhi, data$it16h$hwgt, one_sided = "top")
#' }
iqr_top_bottom_coding <- function(x, w = NULL, times = 3, type = c("type_4", "type_2"), one_sided = NULL) {
    
   
    log_x <- suppressWarnings(log(x))
  
    # Fix log(0) = -Inf
    log_x[log_x == -Inf] <- 0

    # Fix log(negative) = NA
    log_x[is.na(log_x) & !is.na(x)] <- 0
  
    # Compute weighted percentiles
    p_25 <- lissyrtools::compute_weighted_percentiles(log_x, w, probs = 0.25, type = type)
    p_75 <- lissyrtools::compute_weighted_percentiles(log_x, w, probs = 0.75, type = type)
    iqr <- p_75 - p_25
    upper_bound <- p_75 + times * iqr
    lower_bound <- p_25 - times * iqr

    x_coded <- x

    # Apply logic based on `one_sided`
    if (is.null(one_sided)) {
      x_coded <- ifelse(x > exp(upper_bound), exp(upper_bound),
                        ifelse(x < exp(lower_bound), exp(lower_bound), x))
    } else if (one_sided == "top") {
      x_coded <- ifelse(x_coded > exp(upper_bound), exp(upper_bound), x_coded)
    } else if (one_sided == "bottom") {
      x_coded <- ifelse(x_coded < exp(lower_bound), exp(lower_bound), x_coded)
  }
  return(x_coded)
}
