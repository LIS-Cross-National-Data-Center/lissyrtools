#' Compute Absolute Poverty Rate
#'
#' @param data_list A named list of data frames.
#' @param var_name A string specifying the variable name (e.g., "dhi", "pilabour").
#' @param wgt_name A string (optional). The name of the weight variable. If `NULL`, equal weights are assumed.
#' @param daily_poverty_line  A numeric scalar representing the **absolute poverty threshold per day**.
#'   **Note:** This value must be expressed in the **same monetary unit** as the target variable.
#' @param days_in_year Integer. Number of days used to convert the daily poverty line to an annual equivalent. Default is `365`. 
#' @param na.rm Logical. Should missing values be removed before computation? Default is `TRUE`.
#' 
#' @details 
#' This function is intended to compute **absolute poverty** based on **fixed international or national thresholds**.
#' Because the LIS dataset reports **annual income**, the function internally converts the provided daily threshold into an **annual threshold** using `days_in_year`. The default is `365` (calendar year), but can be adjusted.
#' It is **critical** that the daily poverty threshold be expressed in the **same monetary unit** as the target income variable (`var_name`). For example, if income is reported in PPP-adjusted USD, the threshold must also be in PPP-adjusted USD. The function does not perform currency or PPP conversions.
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
#' # Poverty line is defined at 2.15$ 2017 PPP per day, by dafault. 
#' 
#' abs_pvt_rate_215 <- datasets %>% 
#'  map(~ .x %>% filter(!is.na(dhi))) %>%
#'  map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
#'  apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
#'  apply_sqrt_equivalisation("dhi") %>% 
#'  apply_ppp_adjustment("dhi", database = "lis", transformation = "lisppp") %>%  
#'  run_weighted_absolute_poverty("dhi", "new_wgt")
#'  
#' print(abs_pvt_rate_215)  
#'  
#'  # It can be defined to any other threshold. 
#'  
#' abs_pvt_rate_685 <- datasets %>% 
#'  map(~ .x %>% filter(!is.na(dhi))) %>%
#'  map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
#'  apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
#'  apply_sqrt_equivalisation("dhi") %>% 
#'  apply_ppp_adjustment("dhi", database = "lis", transformation = "lisppp") %>%  
#'  run_weighted_absolute_poverty("dhi", "new_wgt", daily_poverty_line = 6.85)
#'  
#' print(abs_pvt_rate_685)   
#' }
run_weighted_absolute_poverty <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  daily_poverty_line = 2.15,
  days_in_year = 365,
  na.rm = TRUE
) {

  # Remove datasets with missing weights
  data_list <- lissyrtools::remove_dname_with_missings_in_weights(
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

  lissyrtools::check_input_in_weight_argument(wgt_name)

  # Convert daily threshold to annual
  annual_poverty_line <- daily_poverty_line * days_in_year

  output_run_absolute_poverty <- purrr::imap(
    data_list,
    ~ {
      var <- .x[[var_name]]
      wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else rep(1, length(var))

      df <- .x
      df$below_poverty <- ifelse(df[[var_name]] < annual_poverty_line, 1, 0)
      weighted_rate <- sum(df$below_poverty * wgt, na.rm = na.rm) / sum(wgt, na.rm = na.rm) * 100
      return(weighted_rate)
    }
  )

  output_run_absolute_poverty <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(
    output_run_absolute_poverty
  )

  return(output_run_absolute_poverty)
}



#' Compute Relative Poverty Rate
#'
#' @param data_list A named list of data frames.
#' @param var_name A string specifying the variable name (e.g., "dhi", "pilabour").
#' @param wgt_name A string (optional). The name of the weight variable. If `NULL`, equal weights are assumed.
#' @param times_median A numeric scalar. The multiple of the median used to define the poverty threshold (default is 0.5).
#' @param type A character vector indicating the percentile estimation type (passed to `compute_weighted_percentiles`). Default is `"type_4"`.
#' @param na.rm Logical. Should missing values be removed before computation? Default is `TRUE`.
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
#' # Poverty line is defined at 50%  of the median value by default. 
#' 
#' rel_pvt_rate_50 <- datasets %>% 
#'  map(~ .x %>% filter(!is.na(dhi))) %>%
#'  map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
#'  apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
#'  apply_sqrt_equivalisation("dhi") %>% 
#'  run_weighted_relative_poverty("dhi", "new_wgt")
#'  
#' print(rel_pvt_rate_50)  
#'  
#'  # It can be defined at other values by specifying the argument `times_median`
#'  
#' rel_pvt_rate_40 <- datasets %>% 
#'  map(~ .x %>% filter(!is.na(dhi))) %>%
#'  map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
#'  apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
#'  apply_sqrt_equivalisation("dhi") %>% 
#'  run_weighted_relative_poverty("dhi", "new_wgt", times_median = 0.4)
#'  
#' print(rel_pvt_rate_40)   
#' }
run_weighted_relative_poverty <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  times_median = 0.5,
  type = c("type_4", "type_2"),
  na.rm = TRUE
) {
  
  # Remove datasets with missing weights
  data_list <- lissyrtools::remove_dname_with_missings_in_weights(
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

  lissyrtools::check_input_in_weight_argument(wgt_name)

  output_run_relative_poverty <- purrr::imap(
    data_list,
    ~ {
      var <- .x[[var_name]]
      wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else rep(1, length(var))

      poverty_line <- times_median *
        lissyrtools::compute_weighted_percentiles(
          var = var,
          wgt = wgt,
          probs = 0.5,
          type = type,
          na.rm = na.rm
        )

      df <- .x
      df$below_poverty <- ifelse(df[[var_name]] < poverty_line, 1, 0)
      weighted_rate <- sum(df$below_poverty * wgt) / sum(wgt) * 100
      return(weighted_rate)
    }
  )
  output_run_relative_poverty <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(
    output_run_relative_poverty
  )
  return(output_run_relative_poverty)
}




#' Compute the Weighted Poverty Gap (Shortfall)
#'
#' Calculates the average shortfall (gap) between individual/household income and the relative poverty line, weighted by the population weights and restricted to those below the poverty line.
#'
#' @param data_list A named list of data frames.
#' @param var_name A string specifying the variable name (e.g., `"dhi"`, `"pilabour"`).
#' @param wgt_name A string (optional). The name of the weight variable. If `NULL`, equal weights are assumed.
#' @param times_median A numeric scalar. The multiple of the median used to define the poverty threshold (default is `0.5`).
#' @param daily_poverty_line A numeric scalar representing the **absolute poverty threshold per day**. (default is `NULL`). 
#' @param type A character vector indicating the percentile estimation type (passed to `compute_weighted_percentiles`). Default is `"type_4"`.
#' @param percent Logical. If `TRUE`, returns the relative shortfall as a percentage of the poverty line. If `FALSE`, returns the absolute daily shortfall (default is `FALSE`).
#' @param na.rm Logical. Should missing values be removed before computation? Default is `TRUE`.

#' @details 
#' When `daily_poverty_line` is specified, the function assumes the threshold is expressed in daily units and converts it to an annual threshold by multiplying it by 365. 
#' It is **critical** that the daily poverty threshold be expressed in the **same monetary unit** as the target income variable (`var_name`). For example, if income is reported in PPP-adjusted USD, the threshold must also be in PPP-adjusted USD. The function does not perform currency or PPP conversions.
#' 
#' 
#' 
#' @return A named list. Each list element is named by country and contains a named numeric vector, where the names are years and the values represent:
#' 
#' - The average daily shortfall in monetary units (if `percent = FALSE`), or
#' - The average relative shortfall in percentage terms (if `percent = TRUE`), among individuals or households below the poverty threshold.
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
#'   map(~ .x %>% filter(!is.na(dhi))) %>%
#'   map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
#'   apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
#'   apply_sqrt_equivalisation("dhi")
#'
#' # Compute average poverty shortfall in daily monetary terms
#' avg_gap <- run_weighted_poverty_shortfall(
#'   data_list = datasets,
#'   var_name = "dhi",
#'   wgt_name = "new_wgt"
#' )
#'
#' # Compute average poverty shortfall in percentage of the poverty line
#' percentage_gap <- run_weighted_poverty_shortfall(
#'   data_list = datasets,
#'   var_name = "dhi",
#'   wgt_name = "new_wgt",
#'   percent = TRUE
#' )
#' 
#' # Compute average poverty shortfall in daily monetary terms, converted to international dollars at 2017 prices. 
#' avg_gap_dollars <- datasets %>% 
#'   apply_ppp_adjustment("dhi", "lis", "lisppp") %>% 
#'   run_weighted_poverty_shortfall(
#'   var_name = "dhi",
#'   wgt_name = "new_wgt"
#' )
#' 
#' # Compute average poverty shortfall in daily monetary terms, from a specified poverty threshold in international dollars at 2017 prices. 
#' avg_gap_absolute_line <- datasets %>% 
#'   apply_ppp_adjustment("dhi", "lis", "lisppp") %>% 
#'   run_weighted_poverty_shortfall(
#'   var_name = "dhi",
#'   wgt_name = "new_wgt", 
#'   daily_poverty_line  = 10
#' )
#' 
#' 
#' print(avg_gap)
#' print(percentage_gap)
#' print(avg_gap_dollars)
#' print(avg_gap_absolute_line)
#' }
run_weighted_poverty_shortfall <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  times_median = 0.5,
  daily_poverty_line  = NULL,
  type = c("type_4", "type_2"),
  percent = FALSE,
  na.rm = TRUE
) {
  
  # Remove datasets with missing weights
  data_list <- lissyrtools::remove_dname_with_missings_in_weights(
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

  lissyrtools::check_input_in_weight_argument(wgt_name)

  # Warning when both `daily_poverty_line ` and `times_median` are specified. 
  if (!is.null(daily_poverty_line ) && times_median != 0.5) {
  warning("Both `daily_poverty_line` and `times_median` are set. Using `daily_poverty_line` and ignoring `times_median`.")
  }


  output_run_poverty_shortfall <- purrr::imap(
    data_list,
    ~ {
      var <- .x[[var_name]]
      wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else rep(1, length(var))

      poverty_line <- if (!is.null(daily_poverty_line )) {
        daily_poverty_line  * 365 # Assuming daily values, convert to yearly
      } else {
        times_median *
          lissyrtools::compute_weighted_percentiles(
            var = var,
            wgt = wgt,
            probs = 0.5,
            type = type,
            na.rm = na.rm
          )
      }

      df <- .x
      df$below_poverty <- ifelse(df[[var_name]] < poverty_line, 1, 0)
      weighted_shortfall <- sum(
        df$below_poverty * wgt * (poverty_line - df[[var_name]])
      ) /
        sum(wgt * df$below_poverty) /
        365

      if (percent == FALSE) {
        return(weighted_shortfall)
      } else {
        relative_shortfall <- weighted_shortfall / (poverty_line / 365) * 100
        return(relative_shortfall)
      }
    }
  )

  output_run_poverty_shortfall <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(
    output_run_poverty_shortfall
  )
  return(output_run_poverty_shortfall)
}



#' Compute the Poverty Gap Index
#'
#' Calculates the Foster–Greer–Thorbecke poverty gap index (FGT1) for a list of datasets. This index measures the intensity of poverty by combining the relative poverty rate with the average income shortfall (as a percentage of the poverty line) among the poor.
#'
#' @param data_list A named list of data frames.
#' @param var_name A string specifying the variable name (e.g., `"dhi"`, `"pilabour"`).
#' @param wgt_name A string. The name of the weight variable.
#' @param times_median A numeric scalar. The multiple of the median used to define the poverty threshold (default is `0.5`).
#' @param daily_poverty_line A numeric scalar representing the **absolute poverty threshold per day**. (default is `NULL`). 
#' @param type A character vector indicating the percentile estimation type (passed to `compute_weighted_percentiles`). Default is `"type_4"`.
#' @param na.rm Logical. Should missing values be removed before computation? Default is `TRUE`.
#'
#' @return A named list. Each list element is named by country and contains a named numeric vector, where the names are years and the values are the computed poverty gap indices (bounded between 0 and 1).
#'
#' @details 
#' This function multiplies the relative poverty rate by the average relative poverty shortfall among the poor, resulting in the FGT1 poverty gap index. The result represents the average poverty gap across the entire population as a fraction of the poverty line.
#' When `daily_poverty_line ` is specified, the function automatically assumes, the scalar is expressed in daily units. 
#' Because the LIS dataset reports **annual income**, the function internally converts the provided daily threshold into an **annual threshold**, multiplying it by `365` (calendar year).
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
#'   map(~ .x %>% filter(!is.na(dhi))) %>%
#'   map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
#'   apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
#'   apply_sqrt_equivalisation("dhi") %>%
#'   apply_ppp_adjustment("dhi", "lis", "lisppp")
#'
#' # Compute the FGT1 poverty gap index
#' pgi <- run_weighted_poverty_gap_index(
#'   data_list = datasets,
#'   var_name = "dhi",
#'   wgt_name = "new_wgt"
#' )
#'
#' # Compute the FGT1 poverty gap index, using an absolut threshold expressed in daily terms. 
#' pgi_daily_threshold <- run_weighted_poverty_gap_index(
#'   data_list = datasets,
#'   var_name = "dhi",
#'   wgt_name = "new_wgt", 
#'   daily_poverty_line = 10
#' )
#' 
#' 
#' print(pgi)
#' print(pgi_daily_threshold)
#' }
run_weighted_poverty_gap_index <- function(
  data_list,
  var_name,
  wgt_name,
  times_median = 0.5,
  daily_poverty_line = NULL,
  type = c("type_4", "type_2"),
  na.rm = TRUE
) {
  
  # Remove datasets with missing weights
  data_list <- lissyrtools::remove_dname_with_missings_in_weights(
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

  lissyrtools::check_input_in_weight_argument(wgt_name)

  # Warning when both `daily_poverty_line ` and `times_median` are specified. 
  if (!is.null(daily_poverty_line) && times_median != 0.5) {
  warning("Both `daily_poverty_line` and `times_median` are set. Using `daily_poverty_line` and ignoring `times_median`.")
  }

  if (!is.null(daily_poverty_line)) {
  
  pvt_rate <- run_weighted_absolute_poverty(
    data_list = data_list,
    var_name = var_name,
    wgt_name = wgt_name,
    daily_poverty_line = daily_poverty_line,
    na.rm = na.rm
  )

  shortfall_percent <- run_weighted_poverty_shortfall(
    data_list = data_list,
    var_name = var_name,
    wgt_name = wgt_name,
    daily_poverty_line = daily_poverty_line,
    type = type,
    percent = TRUE,
    na.rm = na.rm
  )
    
  } else {

  pvt_rate <- run_weighted_relative_poverty(
    data_list = data_list,
    var_name = var_name,
    wgt_name = wgt_name,
    times_median = times_median,
    type = type,
    na.rm = na.rm
  )

  shortfall_percent <- run_weighted_poverty_shortfall(
    data_list = data_list,
    var_name = var_name,
    wgt_name = wgt_name,
    times_median = times_median,
    type = type,
    percent = TRUE,
    na.rm = na.rm
  )
  }

  output_run_pgi <- purrr::map2(pvt_rate, shortfall_percent, ~ .x * .y / 100)

  return(output_run_pgi)
}


