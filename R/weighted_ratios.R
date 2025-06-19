


#' Compute Percentile Ratios
#'
#' This function calculates the ratio between two percentiles (e.g., P90/P10) for each dataset in a list.
#'
#' @param data_list A named list of data frames.
#' @param var_name A string. The name of the variable to analyze (e.g., "dhi", "income").
#' @param wgt_name A string (optional). The name of the weight variable. If `NULL`, equal weights are assumed.
#' @param upper_percentile A numeric scalar (between 0 and 1). The higher percentile (e.g., 0.9).
#' @param lower_percentile A numeric scalar (between 0 and 1). The lower percentile (e.g., 0.1).
#' @param type A character string indicating the percentile type used in `compute_weighted_percentiles`. Defaults to `"type_4"`.
#' @param na.rm Logical. Should missing values be removed before computation? Default is `TRUE`.
#'
#' @return A named list. Each element is named by country and contains a named numeric vector, where names are years and values are the percentile ratios.
#'
#' @details If `upper_percentile` is less than `lower_percentile`, the values are automatically swapped and a warning is issued.
#'
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
#' datasets <- datasets %>% 
#'  map(~ .x %>% filter(!is.na(dhi))) %>%
#'  map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
#'  apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
#'  apply_sqrt_equivalisation("dhi")
#'
#'
#' # Compute dhi  ratios
#' 
#' p90_p10 <- run_weighted_ratios(datasets, var_name = "dhi", wgt_name = "hwgt", 
#'                                 upper_percentile = 0.9, lower_percentile = 0.1)
#'                                 
#' p90_p50 <- run_weighted_ratios(datasets, var_name = "dhi", wgt_name = "hwgt", 
#'                                 upper_percentile = 0.9, lower_percentile = 0.5) 
#'                                                                 
#' p80_p20 <- run_weighted_ratios(datasets, var_name = "dhi", wgt_name = "hwgt", 
#'                                 upper_percentile = 0.8, lower_percentile = 0.2) 
#' print(p90_p10)
#' print(p90_p50) 
#' print(p80_p20)
#' }
run_weighted_ratios <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  upper_percentile,
  lower_percentile,
  type = c("type_4", "type_2"),
  na.rm = TRUE
) {
  
  data_list <- lissyrtools::remove_dname_with_missings_in_weights(
    data_list,
    wgt_name
  ) # return a list cleaned

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
  lissyrtools::check_input_in_weight_argument(wgt_name)

  # Check for invalid inputs in the percentiles, even if already enforced in compute_weighted_percentiles()
  stopifnot(
    upper_percentile >= 0 && upper_percentile <= 1,
    lower_percentile >= 0 && lower_percentile <= 1
  )

  # Check for length 1 in both arguments
  assertthat::assert_that(
    length(upper_percentile) == 1,
    length(lower_percentile) == 1
  )

  # Check order of the percentiles
  if (upper_percentile < lower_percentile) {
    # Swap the values
    temp <- upper_percentile
    upper_percentile <- lower_percentile
    lower_percentile <- temp

    warning(glue::glue(
      "The value of 'upper_percentile' is smaller than 'lower_percentile'. The values have been automatically swapped."
    ))
  }

  # Body of the function

  get_percentiles <- lissyrtools::run_weighted_percentiles(
    data_list = data_list,
    var_name = var_name,
    wgt_name = wgt_name,
    probs = c(lower_percentile, upper_percentile),
    share = FALSE,
    by = NULL,
    type = type
  )

  get_ratios <- purrr::map(
    get_percentiles,
    ~ {
      if (.x[1] == 0) return(NA_real_) else .x[2] / .x[1]
    }
  )

  result <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(get_ratios)
  return(result)
}
