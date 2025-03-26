

#' Inspect the Labels of LIS and LWS variables
#'
#' @param vars A character vector containing LIS/LWS variables or the output list from lissyuse.
#'
#' @return A character vector with the corresponding labels for the selected variables.
#' @export
#'
#' @examples
#' # 1) Without any argument:
#' get_vars_label()
#' 
#' \dontrun{
#' # 2) Using with the outputed list from lissyuse:
#' lis_datasets <- lissyuse(data = c("uk"), vars = c("hpub_i","hpub_u", "hi42", "hi421", "hi422", "hi43"), from = 2016)
#' get_vars_label(vars = lis_datasets)
#' }
#' 
#' # 3) Using a character vector with LIS/LWS variables:
#' get_vars_label(vars = c("fyft", "basb", "hxremit", "bafi1_c", "pasodc"))
get_vars_label <- function(vars = NULL) {
  
  if(is.null(vars)) {
    output <- tibble::deframe(data_vars_labels)
    return(output)
  } 
  
  else if(!(is.null(vars))) {
    if (is.character(vars)) {
      
      valid_vars <- union(lissyrtools::lws_variables, lissyrtools::lis_variables)
      
      invalid_vars <- vars[!vars %in% valid_vars]
      
      if (length(invalid_vars) > 0 & length(invalid_vars) == length(vars)) {
        stop(glue::glue(
          "None of the characters provided in argument `vars` are considered valid. ",
          "Please check that all specified variables exist in 'lissyrtools::lis_variables' or 'lissyrtools::lws_variables'."
        ))
      } else if (length(invalid_vars) > 0) {
        warning(glue::glue(
          "The following variable(s) in the argument `vars` are not valid: {toString(invalid_vars)}. ",
          "Please check that all specified variables exist in 'lissyrtools::lis_variables' or 'lissyrtools::lws_variables'."
        ))
      }
      
      vars_accepted <- vars[!vars %in% invalid_vars] 
      
      output <- tibble::deframe(data_vars_labels)[vars_accepted] 
      return(output)
    }
    
    
    else if (isTRUE(exists("vars"))) {
      
      if (!(is.list(vars))){
        stop(glue::glue(
          "Argument `vars` only accepts lists or character vector."
        )) 
      }
      
      else {
        columns <- names(vars[[1]]) 
        output <- tibble::deframe(data_vars_labels)[columns]
        return(output) 
      }
      
    }
    
    
  }
}

#' Verify if a given variable has a note, for a selected group of countries, in LIS or LWS databases. 
#'
#' @param variable A unit-length character vector containing specified LIS/LWS variables. 
#' @param iso2 A character vector with valid iso2 codes of countries present in LIS/LWS 
#' @param lws A logical value, that guides the tool to search in the LIS or LWS database. The argument is FALSE by defualt, taking LIS as the databse to invetigated if nothing is specified.
#'
#' @return A list, made of character vectors. Each elements corresponds to a country in LIS or LWS databases. 
#' @export
#'
#' @examples
#' variable_has_note(variable = "area_c", iso2 = "br")
#' variable_has_note(variable = "basb", iso2 = c("fr", "de", "us", "uk"), lws = TRUE)
variable_has_note <- function(variable, iso2, lws = FALSE) {
  
  # ensure that argument 'variable' only accepts one character
  
  if (length(variable) > 1) {
    stop(
      glue::glue(
        "Argument `variable` accepts no more than one character."
      )
    )
  }
  
  # ensure the validity of the variable
  
  if (lws) {
    invalid_var <- variable[!variable %in% lissyrtools::lws_variables]
    if (length(invalid_var) > 0) {
      stop(glue::glue("Invalid variable: {paste(invalid_var)} not found in 'lissyrtools::lws_variables'."))
    }
  } else {
    invalid_var <- variable[!variable %in% lissyrtools::lis_variables]
    if (length(invalid_var) > 0) {
      stop(glue::glue("Invalid variable: {paste(invalid_var)} not found in 'lissyrtools::lis_variables'."))
    }
  }
  
  
  
  # ensure the validity of the iso2 codes
  
  valid_iso2 <- if (lws) {
    lissyrtools::show_countries_lws()
  } else {
    lissyrtools::show_countries_lis()
  }
  
  invalid_iso2 <- iso2[!iso2 %in% valid_iso2]
  
  if (length(invalid_iso2) == length(iso2)) {
    # If no valid iso2 codes, stop with an error
    stop(
      glue::glue(
        "None of the provided iso2 codes in argument 'iso2' are valid: {toString(iso2)}. ",
        "Valid codes are stored in lissyrtools::show_countries_{ifelse(lws, 'lws', 'lis')}()."
      )
    )
  } else if (length(invalid_iso2) > 0) {
    # If some codes are invalid, issue a warning
    warning(
      glue::glue(
        "The argument 'iso2' contains invalid iso2 codes: {toString(invalid_iso2)}. ",
        "These iso2 codes are not in the valid list for the selected database. See: lissyrtools::show_countries_{ifelse(lws, 'lws', 'lis')}()."
      )
    )
  }
  
  
  
  # body of the function
  process_country <- function(i) {
    
    db <- if (lws) "LWS" else "LIS"
    get_years_function <- if (lws) lissyrtools::get_years_lws else lissyrtools::get_years_lis
    
    years <- get_years_function(i)[[1]]
    
    existing_years <- data_with_warnings %>%
      dplyr::filter(database == db, iso2 == i, var_name == variable) %>%
      dplyr::pull(year)
    
    year_status <- ifelse(years %in% existing_years, "Yes", "No")
    names(year_status) <- years
    return(year_status)
  }
  
  result <- purrr::map(iso2, process_country)
  names(result) <- iso2
  
  return(result)
}



# On the documentation, one needs to add a visual example with the location of the notes perhaps. 
# METIS > Results > Dataset Information > Code Books > Ctrl + F ("variable")


#' Print the survey used to construct the LIS datasets for a given country.
#'
#' @param iso2 A character vector with valid iso2 codes of countries present in LIS 
#'
#' @return A list, made of character vectors. Each elements corresponds to a country in LIS. 
#' @export
#'
#' @examples
#' get_surveys_lis("it")
#' get_surveys_lis(iso2 = c("uy", "pe"))
get_surveys_lis <- function(iso2) {
  
  # ensure the validity of the iso2 codes
  
  valid_iso2 <- lissyrtools::show_countries_lis()
  invalid_iso2 <- iso2[!iso2 %in% valid_iso2]
  
  if (length(invalid_iso2) == length(iso2)) {
    # If no valid iso2 codes, stop with an error
    stop(
      glue::glue(
        "None of the provided iso2 codes in argument 'iso2' are valid: {toString(iso2)}. ",
        "Valid codes are stored in lissyrtools::show_countries_lis()."
      )
    )
  } else if (length(invalid_iso2) > 0) {
    # If some codes are invalid, issue a warning
    warning(
      glue::glue(
        "The argument 'iso2' contains invalid iso2 codes: {toString(invalid_iso2)}. ",
        "These iso2 codes are not in the valid list for the selected database. See: lissyrtools::show_countries_lis()."
      )
    )
  }
  
  # body of the function
  process_country <- function(i) {
    
  surveys_to_output <- lissyrtools::datasets %>% 
    dplyr::filter(database == "LIS" & iso2 == i) %>%
    dplyr::select(year, survey) %>% 
    dplyr::arrange(year) %>% 
    tibble::deframe()
  
  attributes(surveys_to_output)[1] <- NULL
  
  return(surveys_to_output)
  }
  
  result_list <- purrr::map(iso2,process_country)
  names(result_list) <- iso2
  return(result_list)
}

#' Print the survey used to construct the LWS datasets for a given country.
#'
#' @param iso2 A character vector with valid iso2 codes of countries present in LWS 
#'
#' @return A list, made of character vectors. Each elements corresponds to a country in LWS.
#' @export
#'
#' @examples
#' get_surveys_lws("it")
#' get_surveys_lws(iso2 = c("fr", "jp"))
get_surveys_lws <- function(iso2) {
  
  # ensure the validity of the iso2 codes
  
  valid_iso2 <- lissyrtools::show_countries_lws()
  invalid_iso2 <- iso2[!iso2 %in% valid_iso2]
  
  if (length(invalid_iso2) == length(iso2)) {
    # If no valid iso2 codes, stop with an error
    stop(
      glue::glue(
        "None of the provided iso2 codes in argument 'iso2' are valid: {toString(iso2)}. ",
        "Valid codes are stored in lissyrtools::show_countries_lws()."
      )
    )
  } else if (length(invalid_iso2) > 0) {
    # If some codes are invalid, issue a warning
    warning(
      glue::glue(
        "The argument 'iso2' contains invalid iso2 codes: {toString(invalid_iso2)}. ",
        "These iso2 codes are not in the valid list for the selected database. See: lissyrtools::show_countries_lws()."
      )
    )
  }
  
  # body of the function
  process_country <- function(i) {
    
    surveys_to_output <- lissyrtools::datasets %>% 
      dplyr::filter(database == "LWS" & iso2 == i) %>%
      dplyr::select(year, survey) %>% 
      dplyr::arrange(year) %>% 
      tibble::deframe()
    
    attributes(surveys_to_output)[1] <- NULL
    
    return(surveys_to_output)
  }
  
  result_list <- purrr::map(iso2,process_country)
  names(result_list) <- iso2
  return(result_list)
}




