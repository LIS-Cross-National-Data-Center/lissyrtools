#' Apply Square-Root Equivalisation to a Variable
#'
#' @description
#' This function adjusts a household-level variable (e.g., "dhi", "hicapital") 
#' for household size using a square-root scale equivalisation method. 
#' It divides the variable by the number of household members raised to a power (`eq_scale`), 
#' which defaults to 0.5 (i.e., the square root equivalence scale).
#'
#' @param data_list A named list of data frames, from LIS or LWS microdata.
#' @param var_name A string. The name of the variable to be equivalised.
#' @param eq_scale A numeric value between 0 and 1. The equivalence scale to apply. 
#'        Defaults to `0.5`, corresponding to square-root equivalisation.
#'
#' @return A list of data frames with the equivalised variable.
#'
#' @details 
#' The function assumes that the dataset contains a column named `nhhmem` representing 
#' the number of household members. If this column is missing, the function stops with an error.
#' 
#' It also issues a warning if the target variable is detected as an individual-level variable,
#' since equivalisation is typically applied to household-level measures.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' # Import data, ideally at the level of the variable of interest.
#' data_hhd <- lissyrtools::lissyuse("it", vars = c("dhi"), from = 2010)
#' 
#' # No equivalisation case
#' data_hhd  %>%
#'  purrr::map(~ .x %>% filter(!is.na(dhi))) %>%
#'  run_weighted_mean("dhi", "hpopwgt") 
#' 
#' # Default case: square-root equivalisation
#' data_hhd  %>%
#'  purrr::map(~ .x %>% filter(!is.na(dhi))) %>%
#'  apply_sqrt_equivalisation("dhi") %>% 
#'  run_weighted_mean("dhi", "hpopwgt")
#' }
apply_sqrt_equivalisation <- function(data_list, var_name, eq_scale = 0.5) {
 
  # Check if variable exists in datasets
  assertthat::assert_that(
    var_name %in% names(data_list[[1]]),
    msg = glue::glue("Variable '{var_name}' could not be found in the dataset.")
  )

  assertthat::assert_that(is.numeric(eq_scale) && length(eq_scale) == 1 && eq_scale > 0 && eq_scale < 1,
    msg = glue::glue("Argument `eq_scale` must be a single numeric value.")
  )

  if (eq_scale < 0 && eq_scale > 1) {
    warning(glue::glue("Argument `eq_scale` should be strictly between 0 and 1. Provided: '{eq_scale}'."))
  }

  # Optional warning for likely person-level variable
  if (var_name %in% c(lissyrtools::lis_person_variables, lissyrtools::lws_person_variables)) {
    warning(glue::glue("'{var_name}' appears to be a person-level variable and might not require equivalisation."))
  }

  # Apply equivalisation to each dataset
  result <- purrr::imap(data_list, function(df, name) {
    if (!"nhhmem" %in% names(df)) {
      stop(glue::glue("Column 'nhhmem' is missing."))
    }

    valid <- !is.na(df[[var_name]])  
    df[[var_name]][valid] <- df[[var_name]][valid] / (df[["nhhmem"]][valid] ^ eq_scale)

    return(df)
  })

  return(result)
}


#' Apply OECD Equivalence Scale to a Variable
#'
#' @description
#' This function adjusts a household-level variable (such as income or consumption) 
#' using either the standard or modified OECD equivalence scale. It accounts for 
#' household composition by assigning different weights to additional adults and children.
#' 
#' @param data_list A named list of data frames, from LIS or LWS microdata.
#' @param var_name A string. The name of the variable to be equivalised.
#' @param modified Logical. If `TRUE` (default), the modified OECD scale is used 
#'        (0.5 for additional adults, 0.3 for children). If `FALSE`, the original OECD scale 
#'        is applied (0.7 for additional adults, 0.5 for children).
#' 
#' @return A list of data frames with the equivalised variable. 
#' 
#' @details 
#' The OECD equivalence scale adjusts household income or consumption to reflect 
#' differences in household needs. It requires:
#' - `nhhmem`: total number of household members, (always loaded by default already)
#' - `nhhmem13`: number of household members under 14 years old
#' 
#' The scale is computed as:
#' - **Standard**: 1 + 0.7 × (additional adults) + 0.5 × (children under 14)
#' - **Modified**: 1 + 0.5 × (additional adults) + 0.3 × (children under 14)
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Import data, ideally at the level of the variable of interest.
#' # The variable "nhhmem13" must be imported in advance in order to apply OECD-type equivalisation methods.
#' data_hhd_with_nhhmem13 <- lissyrtools::lissyuse("it", vars = c("dhi", "nhhmem13"), from = 2010)
#' 
#' # No equivalisation case
#' data_hhd  %>%
#'  purrr::map(~ .x %>% filter(!is.na(dhi))) %>%
#'  run_weighted_mean("dhi", "hpopwgt")  
#' 
#' # Default case: modified OECD scale equivalisation
#' data_hhd_with_nhhmem13  %>%
#'  purrr::map(~ .x %>% filter(!is.na(dhi))) %>%
#'  apply_oecd_equivalisation("dhi") %>% 
#'  run_weighted_mean("dhi", "hpopwgt")  
#' 
#' # Standard OECD scale equivalisation
#' data_hhd_with_nhhmem13  %>%
#'  purrr::map(~ .x %>% filter(!is.na(dhi))) %>%
#'  apply_oecd_equivalisation("dhi", modified = FALSE) %>% 
#'  run_weighted_mean("dhi", "hpopwgt") 
#' } 
apply_oecd_equivalisation <- function(data_list, var_name, modified = TRUE) {
  # Check if variable exists in datasets
  assertthat::assert_that(
    var_name %in% names(data_list[[1]]),
    msg = glue::glue("Variable '{var_name}' could not be found in the dataset.")
  )

  assertthat::assert_that(
    all(c("nhhmem", "nhhmem13") %in% names(data_list[[1]])),
    msg = glue::glue(
      "Missing variables: 'nhhmem' and/or 'nhhmem13' must be present in the dataset. ",
      "These are required to calculate the OECD equivalence scale."
    )
  )

  # Optional warning for likely person-level variable
  if (var_name %in% c(lissyrtools::lis_person_variables, lissyrtools::lws_person_variables)) {
    warning(glue::glue("'{var_name}' appears to be a person-level variable and might not require equivalisation."))
  }


  # Apply equivalisation to each dataset
  result <- purrr::imap(data_list, function(df, name) {
    if (!all(c("nhhmem", "nhhmem13") %in% names(df))) {
      stop(glue::glue("Columns 'nhhmem' and 'nhhmem13' must be present in the dataset."))
    }

    n_other_adults <- df[["nhhmem"]] - 1 - df[["nhhmem13"]]

    factor <- if (!modified) {
      1 + (0.7 * n_other_adults) + (0.5 * df[["nhhmem13"]])
    } else {
      1 + (0.5 * n_other_adults) + (0.3 * df[["nhhmem13"]])
    }


    valid <- !is.na(df[[var_name]])  
    df[[var_name]][valid] <- df[[var_name]][valid] / factor

    return(df)
  })

  return(result)
}
