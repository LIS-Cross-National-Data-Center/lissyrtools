



#' Checks for Empty Data Input
#'
#' @description
#' Internal helper to ensure the `data` argument is not `NULL` when required. Suggests loading all countries if input is missing.
#'
#' @param data A character vector.
#' @param lws Logical. If `TRUE`, suggests loading all LWS countries; otherwise, suggests LIS countries.
#'
#' @keywords internal
#' @return Stops with an informative error message if `data` is `NULL`.
check_empty_data <- function(data, lws = FALSE) {
  all_iso2 <- if (lws) {
    "lissyrtools::get_countries_lws()"
  } else {
    "lissyrtools::get_countries_lis()"
  }

  if (is.null(data)) {
    stop(
      glue::glue(
        "The argument 'data' cannot be NULL. 
                 If you wish to load data for all available countries, set 'data' to: {toString(all_iso2)}."
      )
    )
  }
}

#' Checks for the length of the characters in the argument `data`.
#'
#' @description
#' Checks the length of the characters in the argument `data`.
#'
#' @param data A character vector.
#'
#' @keywords internal
#' @return Stops if length of some characters is not 2 or 4, and outputs the invalid characters. 
check_length_iso2 <- function(data) {
  invalid_length_iso2 <- data[!stringr::str_length(data) %in% c(2, 4)]

  if (!all(stringr::str_length(data) %in% c(2, 4))) {
    stop(glue::glue(
      "The argument 'data' must have:
              - A length of 2 for the entire series (e.g., 'it', 'fr'), or
              - A length of 4 for specific datasets (e.g., 'br16', 'it20').
    Invalid iso2 codes: {toString(invalid_length_iso2)}. "
    ))
  }
}

#' Checks for Invalid iso2 codes.
#'
#' @description
#' Internal helper to ensure the iso2 codes in argument `data` correspond to valid iso2 codes.
#'
#' @param data A character vector.
#' @param lws Logical.
#'
#' @keywords internal
#' @return Stops if all iso2 codes in `data` are invalid, and it warns if only some are invalid. 
check_iso2 <- function(data, lws = FALSE) {
  valid_iso2 <- if (lws) {
    lissyrtools::get_countries_lws()
  } else {
    lissyrtools::get_countries_lis()
  }

  data_iso2 <- stringr::str_sub(data, 1, 2)

  invalid_iso2 <- data_iso2[!data_iso2 %in% valid_iso2]

  if (length(invalid_iso2) == length(data_iso2)) {
    # If no valid iso2 codes, stop with an error
    stop(
      glue::glue(
        "None of the provided iso2 codes in 'data' are valid: {toString(data_iso2)}. ",
        "Valid codes are stored in lissyrtools::get_countries_{ifelse(lws, 'lws', 'lis')}()."
      )
    )
  } else if (length(invalid_iso2) > 0) {
    # If some codes are invalid, issue a warning
    warning(
      glue::glue(
        "The argument 'data' contains invalid iso2 codes: {toString(invalid_iso2)}. ",
        "These iso2 codes are not in the valid list for the selected database. See: lissyrtools::get_countries_{ifelse(lws, 'lws', 'lis')}()."
      )
    )
  }
}






#' Checks for Invalid ccyy. 
#'
#' @description
#' Internal helper to ensure the ccyy pairs in argument `data` are valid.
#'
#' @param data A character vector.
#' @param lws Logical.
#'
#' @keywords internal
#' @return Stops if all ccyy codes in `data` are invalid, and it warns if only some are invalid. 
invalid_ccyy_pairs <- function(data, lws = FALSE) {
  database <- if (lws) "LWS" else "LIS"

  valid_pairs <- lissyrtools::datasets %>%
    dplyr::filter(database == !!database) %>%
    dplyr::pull(dname) %>%
    unique()

  invalid_pairs <- data[stringr::str_length(data) == 4][
    !data[stringr::str_length(data) == 4] %in% valid_pairs
  ]

  if (length(invalid_pairs) > 0 & length(data) == length(invalid_pairs)) {
    stop(glue::glue(
      "No country-year pairs in argument 'data' are valid. ",
      "Please double-check your input using `get_()` functions like `get_countries_lis()` or `get_years_lws()`."
    ))
  }

  if (length(invalid_pairs) > 0) {
    warning(glue::glue(
      "The following country-year pairs in argument 'data' are not found in the {database} database: {toString(invalid_pairs)}."
    ))
  }
}

#' Checks for Invalid Vars
#'
#' @description
#' Internal helper to ensure the `vars` argument has no invalid variable names.
#'
#' @param vars A character vector.
#' @param lws Logical.
#'
#' @keywords internal
#' @return Stops if all characters in `vars` are invalid, and it warns if only some are invalid. 
check_invalid_vars <- function(vars, lws = FALSE) {
  valid_vars <- if (lws) {
    lissyrtools::lws_variables
  } else {
    lissyrtools::lis_variables
  }

  invalid_vars <- vars[!vars %in% valid_vars]

  var_source <- if (lws) {
    "lissyrtools::lws_variables"
  } else {
    "lissyrtools::lis_variables"
  }

  if (length(invalid_vars) > 0 & length(invalid_vars) == length(vars)) {
    stop(glue::glue(
      "None of the characters provided in argument `vars` are considered valid. ",
      "Please check that all specified variables exist in {var_source}."
    ))
  } else if (length(invalid_vars) > 0) {
    warning(glue::glue(
      "The following variable(s) in the argument `vars` are not valid: {toString(invalid_vars)}. ",
      "Please check that all specified variables exist in {var_source}."
    ))
  }
}