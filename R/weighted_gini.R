#' Compute Weighted Gini Index Across a List of Data Frames
#' 
#' 
#' @param data_list A named list of data frames, (e.g., across countries or years).
#' @param var_name A string specifying the variable name (e.g., "dhi", "pilabour") to compute the Gini index on.
#' @param wgt_name An optional string specifying the weight variable to be used. If `NULL`, equal weights are assumed.
#' @param na.rm Logical. If `TRUE`, missing values in `var_name` and `wgt_name` are removed. 
#' 
#' 
#' @return A named list. Each list element is named by country and contains a named numeric vector, where the names are years and the values are the computed statistics.
#' @export
#'
#' @examples
#' \dontrun{
#' library(lissyrtools)
#' library(purrr)
#' library(dplyr)
#' 
#' datasets <- lissyrtools::lissyuse(data = c("de", "es", "uk"), vars = c("dhi"), from = 2016)
#' 
#'datasets %>% 
#'  map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%  
#'  apply_iqr_top_bottom_coding("dhi", "hwgt") %>%  
#'  apply_sqrt_equivalisation("dhi") %>% 
#'  run_weighted_gini("dhi", "new_wgt")
#' }
run_weighted_gini <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  na.rm = TRUE
) {
  # Remove datasets with missing weights
  data_list <- lissyrtools::remove_canada_lws_missing_weights_in_p_file(
    data_list,
    wgt_name
  )

  # Check that var_name exists
  assertthat::assert_that(
    var_name %in% names(data_list[[1]]),
    msg = glue::glue(
      "Variable '{var_name}' could not be found as a column name in the datasets."
    )
  )

  # Check that wgt_name exists, if provided
  if (!is.null(wgt_name)) {
    assertthat::assert_that(
      wgt_name %in% names(data_list[[1]]),
      msg = glue::glue(
        "Weight variable '{wgt_name}' could not be found as a column name in the datasets."
      )
    )
  }

  if (!is.null(wgt_name) && !stringr::str_detect(wgt_name, "wgt")) {
    warning(
      "LIS advice: Please check whether you have used one of the following variables in the `wgt_name` argument:\n",
      "  - \"hwgt\", \"hpopwgt\", \"hwgta\", \"pwgt\", \"ppopwgt\", or \"pwgta\".\n\n",
      "If your data was loaded at the household level instead of the person level, you may want to generate a multiple of one of these variables, such as `nhhmem * hwgt`."
    )
  }

  output_run_weighted_gini <- purrr::imap(
    data_list,
    ~ {
      var <- .x[[var_name]]
      wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else rep(1, length(var))

      if (na.rm) {
        keep <- !is.na(var) & !is.na(wgt)
        var <- var[keep]
        wgt <- wgt[keep]
      }

      ovar <- order(var)
      var <- var[ovar]
      wgt <- wgt[ovar]

      wgt <- wgt / sum(wgt)
      p <- cumsum(wgt)
      nu <- cumsum(wgt * var)
      nu <- nu / tail(nu, 1)

      sum(nu[-1] * p[-length(p)]) - sum(nu[-length(nu)] * p[-1])
    }
  )

  output_run_weighted_gini <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(
    output_run_weighted_gini
  )
  return(output_run_weighted_gini)
}