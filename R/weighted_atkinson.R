#' Compute Weighted Atkinson Across a List of Data Frames
#' 
#' 
#' @param data_list A named list of data frames, (e.g., across countries or years).
#' @param var_name A string specifying the variable name (e.g., "dhi", "pilabour") to compute the Atkinson index on.
#' @param wgt_name An optional string specifying the weight variable to be used. If `NULL`, equal weights are assumed.
#' @param epsilon A positive inequality aversion parameter. Must be greater than 0.
#' @param na.rm Logical. If `TRUE`, missing values in `var_name` and `wgt_name` are removed. 
#' 
#' 
#' @return A named list. Each list element is named by country and contains a named numeric vector, where the names are years and the values are the computed statistics.
#' @export
#'
#' @examples
#' \dontrun{
#' datasets <- lissyrtools::lissyuse(data = c("de", "es", "uk"), vars = c("dhi"), from = 2016)
#' 
#' 
#' datasets %>% 
#'  map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%  
#'  apply_iqr_top_bottom_coding("dhi", "hwgt") %>%  
#'  apply_sqrt_equivalisation("dhi") %>% 
#'  run_weighted_atkinson("dhi", "new_wgt", epsilon = 0.5)
#'  
#'  # Negative values are not allowed in the variable for which we are computing the Atkinson index.
#'  # If we remove the top and bottom coding stage from the example above, we will get an error with a warning regarding the datasets containing negative values.
#'  
#' datasets[1:4] %>% 
#'  map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%  
#'  apply_sqrt_equivalisation("dhi") %>% 
#'  run_weighted_atkinson("dhi", "new_wgt", epsilon = 0.5)  
#' }
run_weighted_atkinson <- function(
    data_list,
    var_name,
    wgt_name = NULL,
    epsilon, 
    na.rm = TRUE
) {
  
  # Remove datasets with missing weights
  data_list <- lissyrtools::remove_dname_with_missings_in_weights(data_list, wgt_name)
  
  # Validate epsilon
  assertthat::assert_that(epsilon > 0, msg = "Argument 'epsilon' must be greater than 0.")
  
  # Validate variable and weight presence
  assertthat::assert_that(var_name %in% names(data_list[[1]]),
                          msg = glue::glue("Variable '{var_name}' could not be found in the datasets."))
  
  if (!is.null(wgt_name)) {
    assertthat::assert_that(wgt_name %in% names(data_list[[1]]),
                            msg = glue::glue("Weight variable '{wgt_name}' could not be found in the datasets."))
  }
  
  lissyrtools::check_input_in_weight_argument(wgt_name)
  
  # Identify datasets with negative values
  neg_datasets <- purrr::imap_chr(data_list, function(.x, .name) {
    var <- .x[[var_name]]
    if (na.rm) var <- var[!is.na(var)]
    if (any(var < 0)) return(.name) else return(NA_character_)
  }) %>% na.omit()
  
  if (length(neg_datasets) > 0) {
    stop(glue::glue("Negative values found in '{var_name}' for the following datasets: {paste(neg_datasets, collapse = ', ')}. Atkinson index is undefined for negative values."))
  }
  
  output_run_weighted_atkinson <- purrr::imap(data_list, ~ {
    var <- .x[[var_name]]
    wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else rep(1, length(var))
    
    if (na.rm) {
      keep <- !is.na(var) & !is.na(wgt)
      var <- var[keep]
      wgt <- wgt[keep]
    }
    
    # Check for negative values — Atkinson index is undefined
    if (any(var < 0)) {
      stop(glue::glue("Negative values found in '{var_name}'. Atkinson index is undefined for negative values."))
    }
    
    # Remove zeros if epsilon >= 1
    if (epsilon >= 1 && any(var == 0)) {
      warning(glue::glue("Zero values in '{var_name}' were removed because epsilon >= 1."))
      keep <- var != 0
      var <- var[keep]
      wgt <- wgt[keep]
    }
    
    # Normalize
    wgt <- wgt / sum(wgt)
    mu <- sum(wgt * var)
    var <- var / mu  # Normalize var so mean is 1
    
    if (epsilon == 1) {
      exp_term <- exp(sum(wgt * log(var)))
      atkinson <- 1 - exp_term
    } else {
      atkinson <- 1 - (sum((var^(1 - epsilon)) * wgt))^(1 / (1 - epsilon))
    }
    
    return(atkinson)
  })
  
  output_run_weighted_atkinson <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(output_run_weighted_atkinson)
  return(output_run_weighted_atkinson)
}
