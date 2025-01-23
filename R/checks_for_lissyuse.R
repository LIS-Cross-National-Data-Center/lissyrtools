

check_empty_data <- function(data, lws = FALSE) {
 
  all_iso2 <- if (lws) "lissyrtools::show_countries_lws()$iso2" else "lissyrtools::show_countries_lis()$iso2"
  
  if (is.null(data)) {
    stop(
      glue::glue("The argument 'data' cannot be NULL. 
                 If you wish to load data for all available countries, set 'data' to: {toString(all_iso2)}.")
    )
  }
}  



check_length_iso2 <- function(data) {
  
  invalid_length_iso2 <- data[!str_length(data) %in% c(2,4)]

  if (!all(stringr::str_length(data) %in% c(2, 4))) {
  stop(glue::glue(
    "The argument 'data' must have:
              - A length of 2 for the entire series (e.g., 'it', 'fr'), or
              - A length of 4 for specific datasets (e.g., 'br16', 'it20').
    Invalid iso2 codes: {toString(invalid_length_iso2)}. "
  ))
  } 
}


check_iso2 <- function(data, lws = FALSE) {
  
  valid_iso2 <- if (lws) {
    lissyrtools::show_countries_lws()[["iso2"]]
  } else {
    lissyrtools::show_countries_lis()[["iso2"]]
  }
  
  data_iso2 <- stringr::str_sub(data, 1, 2)
  
  invalid_iso2 <- data_iso2[!data_iso2 %in% valid_iso2]
  
  if (length(invalid_iso2) == length(data_iso2)) {
    # If no valid iso2 codes, stop with an error
    stop(
      glue::glue(
        "None of the provided iso2 codes in 'data' are valid: {toString(data_iso2)}. ",
        "Valid codes are stored in lissyrtools::show_countries_{ifelse(lws, 'lws', 'lis')}()."
      )
    )
  } else if (length(invalid_iso2) > 0) {
    # If some codes are invalid, issue a warning
    warning(
      glue::glue(
        "The argument 'data' contains invalid iso2 codes: {toString(invalid_iso2)}. ",
        "These iso2 codes are not in the valid list for the selected database. See: lissyrtools::show_countries_{ifelse(lws, 'lws', 'lis')}()."
      )
    )
  }
}


invalid_ccyy_pairs <- function(data, lws = FALSE) {
  
  database <- if (lws) "LWS" else "LIS"
  
  valid_pairs <- lissyrtools::datasets %>%
    filter(database == !!database) %>%
    pull(dname) %>%
    unique()
  
  invalid_pairs <- data[stringr::str_length(data) == 4][
    !data[stringr::str_length(data) == 4] %in% valid_pairs
  ]
  
  if (length(invalid_pairs) > 0 & length(data) == length(invalid_pairs)) {
    stop(glue::glue(
      "No country-year pairs in argument 'data' are valid. ",
      "On your local machine, please check the table `lissyrtools::datasets`."
    ))
  }
  
  if (length(invalid_pairs) > 0) {
    warning(glue::glue(
      "The following country-year pairs in argument 'data' are not found in the {database} database: {toString(invalid_pairs)}."
    ))
  }
}


check_invalid_vars <- function(vars, lws = FALSE) {
  
  valid_vars <- if (lws) lissyrtools::lws_variables else lissyrtools::lis_variables
  
  invalid_vars <- vars[!vars %in% valid_vars]
  
  var_source <- if (lws) "lissyrtools::lws_variables" else "lissyrtools::lis_variables"
  
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

