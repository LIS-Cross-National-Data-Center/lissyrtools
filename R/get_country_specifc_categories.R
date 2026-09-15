


#' Retrieve the categories of a country-specific variable in LIS/LWS for a given country.
#'
#' @param variable A unit-length character vector containing a LIS/LWS country-specific (with the "_c" suffix) variable. 
#' @param iso2 A character vector with a valid iso2 code for countries present in LIS/LWS. 
#' @param from A numeric value representing the year (inclusive) after which the LIS/LWS datasets should be considered.
#' @param to A numeric value representing the year (inclusive) up to which the LIS/LWS datasets should be considered.
#' @param database A character value. One of "lis" (default), "lws", or "lcs".
#' @param n_categories A logical value indicating whether to output the number of categories of a single country-specific variable, across the entire time series for a given country.
#'
#' @return A list made of character vectors. If n_categories = TRUE, then a list with a numeric vector.
#' @export
#'
#' @examples
#' library(lissyrtools)
#' library(dplyr)
#' 
#' # In years where no data is recorded for a given variable, it is automatically hidden from the output
#' variable_exists(variable = "health_c", iso2 = "it")
#' variable_country_specific_categories(variable = "health_c", iso2 = "it", from = 1995, to = 2020) 
#' 
#' # To retrieve information on LWS datasets
#' variable_country_specific_categories(variable = "bus1_c", iso2 = "fi", lws = TRUE)
#' 
#' # Using the `n_categories` argument
#' variable_country_specific_categories(variable = "region_c", iso2 = "es", n_categories = TRUE)
#' 
#' # To use this function acroos multiples countries one could make use of the `purrr::map()` function 
#' purrr::map(lissyrtools::get_countries_lws(), ~variable_country_specific_categories(variable = "bus1_c", iso2 = .x, lws = TRUE , n_categories = TRUE))
#' 
variable_country_specific_categories <- function(
    variable,
    iso2,
    from = NULL,
    to = NULL,
    database = "lis",
    n_categories = FALSE, 
    ...
) {
  
  dots <- list(...)
  
  if ("lws" %in% names(dots)) {
    
    if (!is.logical(dots$lws) || length(dots$lws) != 1) {
      stop("'lws' must be TRUE or FALSE.", call. = FALSE)
    }
    
    # Was `database` explicitly supplied?
    database_supplied <- "database" %in% names(as.list(match.call()))
    
    # What database does the old `lws` argument imply?
    old_database <- if (isTRUE(dots$lws)) "lws" else "lis"
    
    # If database was explicitly supplied, check for a conflict
    if (database_supplied && database != old_database) {
      stop(
        "The deprecated argument 'lws' conflicts with ",
        "database = \"", database, "\". ",
        "Please use 'database' instead.",
        call. = FALSE
      )
    }
    
    # Backwards compatibility: lws determines database
    database <- old_database
    
    warning(
      "The argument 'lws' is deprecated and no longer used. ",
      "Please use 'database' instead, which accepts ",
      "\"lis\" (default), \"lws\", or \"lcs\".",
      call. = FALSE
    )
  }
  
  database <- match.arg(database, c("lis", "lws", "lcs"))
  assertthat::assert_that(database %in% c("lis", "lws", "lcs"),
                          msg = glue::glue("'database' must be one of 'lis', 'lws', or 'lcs'. Got '{database}' instead."))
  
  db_upper <- toupper(database)
  
  # ensure that argument 'variable' and 'iso2' only accepts one character
  
  if (length(variable) > 1) {
    stop(
      glue::glue(
        "Argument `variable` accepts no more than one character."
      )
    )
  }
  
  if (
    !(variable %in%
      Reduce(union, list(
        lissyrtools::lis_country_specific_variables,
        lissyrtools::lws_wealth_country_specific_variables,
        lissyrtools::lcs_country_specific_variables
      ))) ==
    TRUE
  ) {
    stop(
      glue::glue(
        paste0(
          "The selected variable: ",
          variable,
          ", is not considered as a country-specific variable in LIS, LWS, or LCS databases.
                Please check: Reduce(union, list(lissyrtools::lis_country_specific_variables, lissyrtools::lws_wealth_country_specific_variables, lissyrtools::lcs_country_specific_variables))."
        )
      )
    )
  }
  
  if (length(iso2) > 1) {
    # specifically for the function for the country-specific categories
    stop(
      glue::glue(
        "Argument `iso2` accepts no more than one character."
      )
    )
  }
  
  # ensure the validity of the variable
  
  if (database == "lws") {
    invalid_var <- variable[!variable %in% lissyrtools::lws_variables]
    if (length(invalid_var) > 0) {
      stop(glue::glue(
        "Invalid variable: {paste(invalid_var)} not found in 'lissyrtools::lws_variables'."
      ))
    }
  } else if (database == "lcs") {
    invalid_var <- variable[!variable %in% lissyrtools::lcs_variables]
    if (length(invalid_var) > 0) {
      stop(glue::glue(
        "Invalid variable: {paste(invalid_var)} not found in 'lissyrtools::lcs_variables'."
      ))
    }
  } else {
    invalid_var <- variable[!variable %in% lissyrtools::lis_variables]
    if (length(invalid_var) > 0) {
      stop(glue::glue(
        "Invalid variable: {paste(invalid_var)} not found in 'lissyrtools::lis_variables'."
      ))
    }
  }
  
  # ensure the validity of the iso2 codes
  
  valid_iso2 <- if (database == "lws") {
    lissyrtools::get_countries_lws()
  } else if (database == "lcs") {
    lissyrtools::get_countries_lcs()
  } else {
    lissyrtools::get_countries_lis()
  }
  
  invalid_iso2 <- iso2[!iso2 %in% valid_iso2]
  
  if (length(invalid_iso2) == length(iso2)) {
    # If no valid iso2 codes, stop with an error
    stop(
      glue::glue(
        "None of the provided iso2 codes in argument 'iso2' are valid: {toString(iso2)}. ",
        "Valid codes are stored in lissyrtools::get_countries_{database}()."
      )
    )
  }
  
  # auxilliary step to get dnames
  existing_dname <- lissyrtools::variable_exists(variable, iso2, database)[[1]]
  existing_dname <- names(existing_dname[existing_dname == "Yes"])
  
  valid_dnames <- lissyrtools::datasets %>%
    dplyr::filter(database == db_upper) %>%
    dplyr::filter(if (!is.null(from)) year >= from else TRUE) %>%
    dplyr::filter(if (!is.null(to)) year <= to else TRUE) %>%
    dplyr::filter(iso2 == {{ iso2 }}) %>%
    dplyr::filter(year %in% existing_dname) %>%
    dplyr::select(dname) %>%
    dplyr::pull()
  
  if (length(valid_dnames) == 0) {
    warning(
      glue::glue(
        paste0(
          "The selected variable: ",
          variable,
          ", does not have values other than zeros or missings for the selectd years."
        )
      )
    )
  }
  
  # auxilliary step to names of the elements of the list
  
  names_of_the_list <- lissyrtools::value_label_c_data %>%
    dplyr::filter(database == db_upper) %>%
    dplyr::filter(dname %in% valid_dnames) %>%
    dplyr::left_join(
      lissyrtools::datasets %>%
        dplyr::filter(database == db_upper) %>%
        dplyr::select(dname, year),
      by = "dname"
    ) %>%
    dplyr::group_by(database, dname, year, var_name) %>%
    dplyr::filter(iso2 == {{ iso2 }} & var_name == {{ variable }}) %>%
    dplyr::summarize(
      alternative_label = paste(unique(alternative_label), collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(year)) %>%
    dplyr::mutate(
      to_name_the_list = paste0(dname, " - ", alternative_label)
    ) %>%
    dplyr::select(to_name_the_list) %>%
    dplyr::pull()
  
  # body of the function
  if (n_categories == FALSE) {
    process_dnames <- function(i) {
      output_vector <- lissyrtools::value_label_c_data %>%
        dplyr::filter(database == db_upper) %>%
        dplyr::filter(dname == i & var_name == {{ variable }}) %>%
        dplyr::select(value_label) %>%
        dplyr::pull()
      
      names(output_vector) <- lissyrtools::value_label_c_data %>%
        dplyr::filter(database == db_upper) %>%
        dplyr::filter(dname == i & var_name == {{ variable }}) %>%
        dplyr::select(code) %>%
        dplyr::pull()
      
      return(output_vector)
    }
    
    result <- purrr::map(valid_dnames, process_dnames)
    names(result) <- names_of_the_list
    return(result)
  } else if (n_categories == TRUE) {
    output_data <- lissyrtools::value_label_c_data %>%
      dplyr::filter(database == db_upper) %>%
      dplyr::filter(dname %in% valid_dnames & var_name == {{ variable }}) %>%
      dplyr::left_join(
        lissyrtools::datasets %>%
          dplyr::filter(database == db_upper) %>%
          dplyr::select(dname, year),
        by = "dname"
      ) %>%
      dplyr::group_by(dname, year) %>%
      dplyr::summarize(
        distinct_value_count = dplyr::n_distinct(value_label),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(year))
    
    output_values <- output_data %>%
      dplyr::select(distinct_value_count) %>%
      dplyr::pull()
    
    names(output_values) <- output_data %>%
      dplyr::select(dname) %>%
      dplyr::pull()
    
    result <- list()
    result[[1]] <- output_values
    names(result) <- paste0(
      db_upper, " ",
      "database. Number of distinct categories in variable: ",
      variable,
      "."
    )
    
    return(result)
  }
}
